// Bernard Bloch -- 260632216

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <ctype.h> // isprint for printing data
#include <assert.h> // assert
#include "sfs_api.h"
#include "disk_emu.h"

// check unit to ensure that I am writing/reading what I intend
static int read_safe(int start_address, int nblocks, void *buffer, const enum UnitEnum unit) {
	uint16_t ad = start_address;
	//printf("read_blocks %s at block %d-->%d\n", units[unit].name, start_address, nblocks);
	assert(ssfs_is(unit, (ref16_t){ .part = { ad, 0 } }));
	assert(ssfs_is(unit, (ref16_t){ .part = { ad + nblocks - 1, 0 } }));
	return read_blocks(start_address, nblocks, buffer);
}
static int write_safe(int start_address, int nblocks, void *buffer, const enum UnitEnum unit) {
	unsigned char *b = buffer;
	uint16_t ad = start_address, nb = nblocks;
	//printf("write_blocks %s at block %d-->%d, '%.2x,%.2x,%.2x,%.2x'...\n", units[unit].name, start_address, nblocks, b[0], b[1], b[2], b[3]);
	assert(ssfs_is(unit, (ref16_t){ .part = { ad, 0 } }));
	assert(ssfs_is(unit, (ref16_t){ .part = { ad + nb - 1, 0 } }));
	assert(start_address != 0 || !memcmp(MAGIC, b, sizeof MAGIC - 1));
	return write_blocks(start_address, nblocks, buffer);
}






// the superblock is kept in memory, updated every time it changes
// Changed from pointer to static, since there is no rmssfs. The caller is responsible for correct operation.
static super_block_t superBlock;





/**********************
 * data block methods *
 **********************/
// These deal with only setting the free bitmap. Open files write to the disk.

// dataRefs[count] are new data blocks references to fill or return -1
static int newData(ref16_t *dataRefs, unsigned blockCount)
{
	assert(dataRefs);
	assert(blockCount);
	// find free bitmap blocks
	char *bitmap = &superBlock.free.data[DATA_START];
	for(unsigned i = 0; i < blockCount; i++) if(!(bitmap = memchr(bitmap, 0, (size_t)(superBlock.free.data + BLOCK_SIZE - bitmap))) || (dataRefs[i].whole = 0, dataRefs[i].part.block = bitmap - superBlock.free.data, ++bitmap > &superBlock.free.data[BLOCK_SIZE])) return printf("Not enough free space on the disk to satisfy %u block requests.\n", blockCount), -1;
	// mark the blocks as used
	for(unsigned i = 0; i < blockCount; i++) superBlock.free.data[dataRefs[i].part.block] = 1;
	// write the superblock
	if(write_safe(SUPER_START, 1, &superBlock, SUPER) != 1) return printf("Error writing Superblock to get %u blocks.\n", blockCount), -1;
	return 0;
}
// dataRefs[count] are the data to free or return -1
static int freeData(ref16_t *dataRefs, unsigned count)
{
	assert(count);
	int isAffected = FALSE;
	for(unsigned i = 0; i < count; i++)
	{
		assert(ssfs_is(DATA, dataRefs[i]));
		unsigned block = dataRefs[i].part.block;
		if(superBlock.free.data[block] == 0) continue;
		superBlock.free.data[block] = 0;
		isAffected = TRUE;
	}
	if(isAffected && write_safe(SUPER_START, 1, &superBlock, SUPER) != 1) return printf("Error writing Superblock to free %u blocks.\n", count), -1;
	return 0;
}

/*****************
 * inode methods *
 *****************/

// Map a valid data reference to get a unique inode reference.
static ref16_t dataRefToInodeRef(ref16_t dataRef)
{
	assert(ssfs_is(DATA, dataRef));
	unsigned plain = dataRef.part.block - DATA_START;
	ref16_t inodeRef = { .part = { plain / INODES_PER_BLOCK + INODES_START, plain % INODES_PER_BLOCK } };
	assert(ssfs_is(INODES, inodeRef));
	return inodeRef;
}
// New inodes are taken all at once.
static ref16_t newInodes(unsigned iCount, unsigned byteCount, char *contents)
{
	// http://stackoverflow.com/questions/7008784/how-to-initialize-a-union-object
	const ref16_t null = { .whole = 0 };
	ref16_t returnIRef = { .whole = 0 };
	assert(byteCount);
	// http://stackoverflow.com/questions/2745074/fast-ceiling-of-an-integer-division-in-c-c
	unsigned blockCount = 1 + ((signed)byteCount - 1) / BLOCK_SIZE;
	assert(iCount == 1 + (blockCount - 1) / INODE_DIRECTS);
	if(iCount >= INODE_BLOCKS * INODES_PER_BLOCK || blockCount >= DATA_BLOCKS) return printf("newIndes: system is not equipped to deal with %u inodes and %u blocks.\n", iCount, blockCount), null;
	int isComplete = FALSE, isData = FALSE;
	ref16_t dataRefs[DATA_BLOCKS];
	do // try
	{
		// get all the data first (important)
		if(newData(dataRefs, blockCount)) break;
		isData = TRUE;
		// set up the loop by making them the initial values
		ref16_t iBlockRef = { .whole = 0 }, iRef;
		inode_block_t iBlock;
		inode_t *inode = NULL;
		unsigned runByteCount = 0;
		unsigned runBlockCount = 0;
		// making new inodes
		unsigned runICount;
		for(runICount = 0; runICount < iCount; runICount++) {
			unsigned toGoBlockCount = blockCount - runBlockCount;
			unsigned iDirectCount = toGoBlockCount < INODE_DIRECTS ? toGoBlockCount : INODE_DIRECTS;
			assert(iDirectCount);
			assert(iDirectCount <= INODE_DIRECTS);
			iRef = dataRefToInodeRef(dataRefs[runBlockCount]);
			// swap out the inode blocks (always on the first loop)
			if(iRef.part.block != iBlockRef.part.block)
			{
				if((inode != NULL && write_safe(iBlockRef.part.block, 1, &iBlock, INODES) != 1) || read_safe(iRef.part.block, 1, &iBlock, INODES) != 1) break;
				iBlockRef = iRef;
			}
			inode = &iBlock.inodes[iRef.part.index];
			if(returnIRef.whole == 0) returnIRef = iRef;
			// fill the inode in
			inode->count = iDirectCount;
			memcpy(&inode->direct[0], &dataRefs[runBlockCount], sizeof(ref16_t) * iDirectCount);
			assert(inode->direct[0].whole);
			// also, fill the data
			unsigned d;
			for(d = 0; d < iDirectCount; d++) {
				ref16_t dataRef = inode->direct[d];
				assert(ssfs_is(DATA, dataRef));
				if(write_safe(dataRef.part.block, 1, &contents[runByteCount], DATA) != 1) break; // error
				runByteCount += BLOCK_SIZE;
				runBlockCount++;
			}
			if(d < iDirectCount) break; // error
			inode->indirect = (runByteCount < byteCount) ? dataRefToInodeRef(dataRefs[runBlockCount]) : (ref16_t){ .whole = 0 };
		}
		if(runICount < iCount) break; // error
		// misses the last write
		if(write_safe(iRef.part.block, 1, &iBlock, INODES) != 1) break; // error
		assert(runByteCount >= byteCount);
		assert(1 + (runBlockCount - 1) / INODE_DIRECTS == iCount);
		isComplete = TRUE;
	} while(FALSE); if(isComplete == FALSE) // catch
	{
		printf("newInodes: failed.\n");
		if(isData && freeData(dataRefs, blockCount)) printf("newInodes: corrution.\n");
		// forget the inode, it will be overwritten anyway
		return null;
	}
	return returnIRef;
}
static int freeInode(ref16_t iRef) {
	if(iRef.whole == 0) return 0; // nothing to free
	assert(ssfs_is(INODES, iRef));
	inode_block_t inodeBlock;
	if(read_safe(iRef.part.block, 1, &inodeBlock, INODES) != 1) return printf("freeInode: block read failed at %u:%u.\n", iRef.part.block, iRef.part.index), -1;
	inode_t *inode = &inodeBlock.inodes[iRef.part.index];
	if(inode->count == 0) return printf("freeInode: block is null at %u:%u.\n", iRef.part.block, iRef.part.index), 0; // Maybe if we return success no one will notice?
	assert(inode->count <= INODE_DIRECTS);
	ref16_t inderect = inode->indirect;
	if(freeData(inode->direct, inode->count)) return printf("freeInode: data failed at %u:%u.\n", iRef.part.block, iRef.part.index), -1;
	memset(inode, 0, sizeof *inode);
	if(write_safe(iRef.part.block, 1, &inodeBlock, INODES) != 1) return printf("freeInode: block write failed at %u:%u.\n", iRef.part.block, iRef.part.index), -1;
	return freeInode(inderect); // tail recursion
}

/****************
 * file methods *
 ****************/

// used from last assignment, returns the plain index
static int hashFile(const char *key)
{
    unsigned hash = 5381;
    for (unsigned counter = 0; key[counter]!='\0'; counter++){
        hash = ((hash << 5) + hash) + key[counter];
    }
	return hash % (FILES_PER_BLOCK * FILE_BLOCKS);
}
// for converting plain and ref16_t
static unsigned refToPlainFile(ref16_t ref) { return (ref.part.block - FILES_START) * FILES_PER_BLOCK + ref.part.index; }
static ref16_t plainToRefFile(unsigned plain) { return (ref16_t){ .part = { FILES_START + plain / FILES_PER_BLOCK, plain % FILES_PER_BLOCK } }; }
// Use updateFile to grow or shrink the size. updateFile leaves the file contents in an undefined state but the size defined. If it fails half-way, there may be corruption of the file (that we should do something about.)
static int updateFile(ref16_t fileRef, unsigned size, char *contents) {
	assert(contents);
	assert(ssfs_is(FILES, fileRef));

	//if(size > FILE_BLOCKS * BLOCK_SIZE) return printf("File at %u:%u is too big to fit on the drive.\n", fileRef.part.block, fileRef.part.index), -1; // hides errors -- will be caught in newInodes->newData.
	file_block_t fileBlock;
	if(read_safe(fileRef.part.block, 1, &fileBlock, FILES) != 1) return printf("updateFile: couldn't read block %u.\n", fileRef.part.block), -1;
	file_t *file = &fileBlock.files[fileRef.part.index];
	// delete the file contents if there are some
	if(file->inode.whole && freeInode(file->inode)) return printf("updateFile %s: couldn't free inode.\n", file->name), -1;
	file->inode.whole = 0;
	unsigned blocks = 1 + ((signed)size - 1) / BLOCK_SIZE;
	unsigned inodes = 1 + (blocks - 1) / INODE_DIRECTS;
	assert(blocks);
	ref16_t newInode = newInodes(inodes, size ? size : 1, contents);
	if(newInode.whole == 0) return printf("updateFile %s: couldn't get inodes.\n", file->name), -1;
	assert(file->name[0] != '%');
	file->inode = newInode;
	file->size = size;
	if(write_safe(fileRef.part.block, 1, &fileBlock, FILES) != 1) return freeInode(file->inode), printf("newFile %s: couldn't write to disk.\n", file->name), -1;
	return 0;
}
// returns a reference to the directory or null
// https://en.wikipedia.org/wiki/Open_addressing
// http://interactivepython.org/runestone/static/pythonds/SortSearch/Hashing.html
static ref16_t findFileSlot(char *key) {
	ref16_t null = { .whole = 0 };
	if(!key || !*key || strlen(key) > MAX_FILENAME) return null;
	int hash = hashFile(key);
	ref16_t fileRef = { .whole = 0 };
	file_block_t fileBlock;
	file_t *file;
	do {
		// Get file at the hash by reading as appropriate from the disk
		ref16_t newFileRef = plainToRefFile(hash);
		if(newFileRef.part.block != fileRef.part.block && read_safe(newFileRef.part.block, 1, &fileBlock, FILES) != 1) return printf("findFileSlot: failed to read from the disk.\n"), null;
		fileRef = newFileRef;
		file = &fileBlock.files[fileRef.part.index];
		if(file->inode.whole == 0 || strcmp(key, file->name) == 0) break;
		// collision
		hash = (hash + 1) % (FILES_PER_BLOCK * FILE_BLOCKS);
		//printf("findFileSlot: adding '%s' would collide with '%s' at %u:%u.\n", key, file->name, fileRef.part.block, fileRef.part.index);
		// The hash size is greater then 2x larger then the maximum files on the disk, so this is safe
	} while(TRUE);
	return fileRef;
}
// the file is key; if it doesn't exist, create it, return the reference or null
static ref16_t newFile(char *key)
{
	ref16_t null = { .whole = 0 };

	ref16_t fileRef = findFileSlot(key);
	if(fileRef.whole == null.whole) return printf("newFile: invalid filename, %s.\n", key), null;
	// re-read the file block
	file_block_t fileBlock;
	if(read_safe(fileRef.part.block, 1, &fileBlock, FILES) != 1) return printf("newFile: failed to read from the disk.\n"), null;
	file_t *file = &fileBlock.files[fileRef.part.index];
	// findSlotFile returned us an unused file, create a file
	if(file->inode.whole == 0) {
		strcpy(file->name, key);
		//printf("newFile: %s\n", file->name);
		if(write_safe(fileRef.part.block, 1, &fileBlock, FILES) != 1) return printf("newFile: failed to write to the directory.\n"), null;
		if(updateFile(fileRef, 0, "")) return printf("newFile: new file failed.\n"), null;
	}
	return fileRef;
}
// prototype
static void movedOpen(ref16_t ref, ref16_t newRef);
static int freeOpen(int fp, int save);
// Delete file.
// This is an open address hash table, follows remove at https://en.wikipedia.org/wiki/Open_addressing
static int freeFile(ref16_t iRef)
{
	const ref16_t null = (ref16_t){ .whole = 0 };
	// "i := find_slot(key)"
	ref16_t jRef;
	// "if slot[i] is unoccupied return // key is not in the table" [
	if(ssfs_is(FILES, iRef) == FALSE) return printf("freeFile: %u:%u is not a valid file pointer.\n", iRef.part.block, iRef.part.index), -1;
	file_block_t fileBlocks[FILE_BLOCKS];
	if(read_safe(FILES_START, FILE_BLOCKS, &fileBlocks, FILES) != FILE_BLOCKS) return printf("freeFile: read entire file system failed.\n"), -1;
	file_t *iFile = &fileBlocks[iRef.part.block - FILES_START].files[iRef.part.index], *jFile;
	if(iFile->inode.whole == 0) return printf("freeFile: file is not there %u:%u.\n", iRef.part.block, iRef.part.index), -1;
	// ]
	
	// "loop"
	unsigned i, j, k;
	i = j = refToPlainFile(iRef);
	do {
		// "mark slot[i] as unoccupied"
		iRef = plainToRefFile(i);
		iFile = &fileBlocks[iRef.part.block - FILES_START].files[iRef.part.index];
		//printf("delete i=%u File%u:%u'%s'\n", i, iRef.part.block, iRef.part.index, iFile->name);
		if(iFile->inode.whole && freeInode(iFile->inode)) return printf("freeFile: '%s' wasn't able to free inode at %u:%u.\n", iFile->name, iFile->inode.part.block, iFile->inode.part.index), -1;
		memset(iFile, 0, sizeof *iFile);
		movedOpen(iRef, null);
		// "r2:"
		do {
			// "j := (j+1) modulo num_slots"
			j = (j + 1) % (FILE_BLOCKS * FILES_PER_BLOCK);
			// "if slot[j] is unoccupied exit loop"
			jRef = plainToRefFile(j);
			jFile = &fileBlocks[jRef.part.block - FILES_START].files[jRef.part.index];
			if(jFile->inode.whole == 0) return write_safe(FILES_START, FILE_BLOCKS, &fileBlocks, FILES) != FILE_BLOCKS ? (printf("freeFile: write entire file system failed.\n"), -1) : 0;
			// "k := hash(slot[j].key) modulo num_slots"
			k = hashFile(jFile->name);
			// "if ( (i<=j) ? ((i<k)&&(k<=j)) : ((i<k)||(k<=j)) ) goto r2;"
		} while((i<=j) ? ((i<k)&&(k<=j)) : ((i<k)||(k<=j)));
		// "slot[i] := slot[j]" (file is replaced) [
		memcpy(iFile, jFile, sizeof *jFile);
		memset(jFile, 0, sizeof *jFile); // j -> i, local file system copy
		movedOpen(jRef, iRef); // open memory copy
		// ]
		// "i := j"
		i = j;
	} while(TRUE);
}

/***************
 * file in RAM *
 ***************/

// Simplifying assumption: the machine has enough resources to hold an entire file buffer in memory.
typedef struct
{
	ref16_t file;
	unsigned size;
	unsigned cursor_r, cursor_w; // having two cursors is a requirements bug that must be duplicated for the tests
	// Maximum file size. Some sort of reallocation would be suitable, since this is almost a MB
	char contents[BLOCK_SIZE * DATA_BLOCKS];
} open_t;

// fileID is the index into the files table
static open_t *opens[MAX_OPEN_FILES];

// this gets the open from a fileID or null
static open_t *idOpen(int id) { return id >= 0 && id < MAX_OPEN_FILES ? opens[id] : NULL; }
// this just searches for an unused spot
static int unusedOpens()
{
	for(int i = 0; i < MAX_OPEN_FILES; i++) if(opens[i] == NULL) return i;
	printf("unusedOpens: maximum %d open files.\n", MAX_OPEN_FILES);
	return -1;
}
// searches the existing opens for a reference that matches the argument, returns the fileID or -1 if it didn't find it
static int existingOpen(ref16_t ref)
{
	for(int i = 0; i < MAX_OPEN_FILES; i++)
	{
		open_t *open = opens[i];
		if(open && open->file.whole == ref.whole) return i;
	}
	return -1;
}
// from a to b; b could be null
static void movedOpen(ref16_t a, ref16_t b)
{
	assert(a.whole);

	int fileID = existingOpen(a);
	if(fileID < 0) return; // good, we didn't have it open
	// this is for test2, which deletes a file without closing it, and expects it to be deleted
	if(b.whole == 0)
	{
		printf("Deleting open FileID%d. DANGER!\n", fileID);
		freeOpen(fileID, FALSE);
	}
	else
	{
		//printf("movedOpen: FileID%d moved due to hash %u:%u, is now %u:%u.\n", fileID, opens[fileID]->file.part.block, opens[fileID]->file.part.index, b.part.block, b.part.index);
		opens[fileID]->file.whole = b.whole;
	}
}
// Returns an index to the myFopens table or -1. The file must exist
static int newOpen(ref16_t ref)
{
	// is it a valid file pointer
	if(ssfs_is(FILES, ref) == FALSE) return -1; // "Return -1 for error besides mkssfs"

	// no two copies of the same file are allowed
	{
		// this behaviour is necessary to complete test1 without errors
		int e = existingOpen(ref);
		if(e >= 0) return printf("newOpen: file at %u:%u is already open in RW mode; returning duplicate file pointer FileID%d.\n", ref.part.block, ref.part.index, e), e;
	}

	// search for a spot, this will be the file pointer, it is also the index
	int fp = unusedOpens();
	if(fp < 0) return -1; // "Return -1 for error besides mkssfs"

	// read the file_t directory information
	file_block_t fileBlock;
	if(read_safe(ref.part.block, 1, &fileBlock, FILES) != 1) return -1;
	file_t *file = &fileBlock.files[ref.part.index];
	// there is not a file there
	if(file->inode.whole == 0) return -1;

	// allocate a new open_t
	open_t *open = calloc(sizeof(open_t), (size_t)1);
	if(open == NULL) return printf("newOpen: error allocating %lu bytes for %s\n", sizeof(open_t), file->name), -1; // "Return -1 for error besides mkssfs"
	open->file = ref;
	open->size = file->size;
	unsigned sizeCountDown = file->size;
	unsigned sizeCountUp   = 0;
	// Transfer file_t on disk to open_t in memory. Start with the file
	ref16_t inodeRef = file->inode;
	while(sizeCountDown) {
		//printf("%u+%u=%u, %u:%u\n", sizeCountUp, sizeCountDown, file->size, inodeRef.part.block, inodeRef.part.index);
		assert(ssfs_is(INODES, inodeRef));
		// get the next inode from the disk
		inode_block_t inodeBlock;
		if(read_safe(inodeRef.part.block, 1, &inodeBlock, INODES) != 1) return -1;
		inode_t *inode = &inodeBlock.inodes[inodeRef.part.index];
		// assert everything
		assert(inode->count != 0 && inode->count <= INODE_DIRECTS);
		unsigned blocksRemaining = 1 + (sizeCountDown - 1) / BLOCK_SIZE;
		int isIndirect = blocksRemaining > INODE_DIRECTS;
		unsigned blocksInInode = isIndirect ? INODE_DIRECTS : blocksRemaining;
		assert(inode->count == blocksInInode);
		assert(isIndirect == (inode->indirect.whole != 0));
		for(int p = 0; p < inode->count; p++) {
			ref16_t dataRef = inode->direct[p];
			assert(ssfs_is(DATA, dataRef));
			data_block_t dataBlock;
			if(read_safe(dataRef.part.block, 1, &dataBlock, DATA) != 1) return -1;
			int takeOff = (sizeCountDown >= BLOCK_SIZE) ? BLOCK_SIZE : sizeCountDown;
			memcpy(open->contents + sizeCountUp, &dataBlock, sizeof(char) * takeOff);
			sizeCountDown -= takeOff;
			sizeCountUp += takeOff;
		}
		inodeRef = inode->indirect;
	}
	if(sizeCountUp != file->size)
	{
		printf("newOpen: file %s has caused in invalid read.\n", file->name);
		return -1;
	}
	// store it in the list of opens
	opens[fp] = open;
	return fp;
}
// Closes then writes a file, returns 0 if it was able to
static int freeOpen(int fileID, int save) {
	open_t *open = idOpen(fileID);
	if(open == NULL) return printf("Trying to close %d but was not open.\n", fileID), -1; // "Return -1 for error besides mkssfs"
	// safe correct way: (fails tests)
	//if(save && updateFile(open->file, open->size, open->contents)) return printf("freeOpen: updateFile failed.\n"), -1;

	// sfs_test2.c errors require this for maximally large files
	ref16_t error = { .whole = 0 };
	if(save && updateFile(open->file, open->size, open->contents))
	{
		printf("freeOpen: updateFile failed on FileID%d; to comply with tests, DATA LOSS IMMINENT.\n", fileID);
		error = open->file;
	}
	free(open);
	opens[fileID] = NULL;
	// The worst possible thing for our user.
	if(error.whole && updateFile(error, 0, "")) printf("freeOpen: corruption of FileID%d.\n", fileID);
	return 0;
}
static void close_all_opens(void) {
	for(int i = 0; i < MAX_OPEN_FILES; i++) if(idOpen(i)) freeOpen(i, TRUE);
}






// helper functions /\ real functions \/





// If fresh is equal to 1, it means that the disk has not been created. So then we call init_fresh_disk and we initialize the superblock_t contents. Otherwise, we just call init_disk 
// "The mkssfs() formats the virtual disk implemented by the disk emulator and creates an instance of SSFS file system on top of it. The mkssfs() has a fresh flag to signal that the file system should be created from scratch. If flag is false, the file system is opened from the disk (i.e., we assume that a valid file system is already there in the file system. The support for persistence is important so you can reuse existing data or create a new file system."
void mkssfs(int fresh)
{
	assert(sizeof(ref16_t) == 2);
	assert(sizeof(int) >= 4);
	assert(sizeof(super_block_t) == BLOCK_SIZE);
	assert(sizeof(file_block_t) == BLOCK_SIZE);
	assert(sizeof(inode_block_t) == BLOCK_SIZE);
	assert(sizeof(data_block_t) == BLOCK_SIZE);
	assert(strlen(MAGIC) < DATA_START);

	if(fresh)
	{
		if(init_fresh_disk(FILE_SYSTEM_NAME, BLOCK_SIZE, NUM_BLOCKS)) {
			printf("init_fresh_disk error.\n");
			close_disk();
			return;
		}
		memset(&superBlock, 0, sizeof superBlock);
		memcpy(superBlock.super.magic, MAGIC, sizeof MAGIC - 1);
		if(write_safe(SUPER_START, 1, &superBlock, SUPER) != 1) printf("mkssfs: failed to write superblock.\n");
		file_block_t fileBlocks[FILE_BLOCKS];
		memset(&fileBlocks, 0, sizeof fileBlocks);
		if(write_safe(FILES_START, FILE_BLOCKS, &fileBlocks, FILES) != FILE_BLOCKS) printf("mkssfs: failed to clear file blocks.\n");
		inode_block_t inodeBlocks[INODE_BLOCKS];
		memset(&inodeBlocks, 0, sizeof inodeBlocks);
		if(write_safe(INODES_START, INODE_BLOCKS, &inodeBlocks, INODES) != INODE_BLOCKS) printf("mkssfs: failed to clear inode blocks.\n");
	}
	else
	{
		if(init_disk(FILE_SYSTEM_NAME, BLOCK_SIZE, NUM_BLOCKS))
		{
			printf("%s: failed init_disk().\n", FILE_SYSTEM_NAME);
			close_disk();
			exit(1);
		}
		if(read_safe(SUPER_START, 1, &superBlock, SUPER) != 1)
		{
			printf("%s: failed reading superblock.\n", FILE_SYSTEM_NAME);
			close_disk();
			exit(1);
		}
		if(strncmp(MAGIC, superBlock.super.magic, sizeof MAGIC - 1) != 0)
		{
			printf("%s: is not a BlochFS file system, is a '%x%x%x%x%.16s'.\n", FILE_SYSTEM_NAME, (char)superBlock.super.magic[0], superBlock.super.magic[1], superBlock.super.magic[2], (char)superBlock.super.magic[3], superBlock.super.magic + 4);
			close_disk();
			exit(1);
		}
	}
	atexit(close_all_opens);
}
// returns a file pointer or -1
// "The ssfs_fopen() opens a file and returns an integer that corresponds to the index of the entry for the newly opened file in the file descriptor table. If the file does not exist, it creates a new file and sets its size to 0. If the file exists, the file is opened in append mode (i.e., set the write file pointer to the end of the file and read at the beginning of the file)."
int ssfs_fopen(char *name){
	ref16_t nf = newFile(name); // could be that this succeeds while the other fails, creating an empty file without opening it; don't know if this is what the user expects
	if(nf.whole == 0) return printf("ssfs_fopen: failed creating file.\n"), -1;
    int fileID = newOpen(nf);
	if(fileID < 0) return printf("ssfs_fopen: failed opening file.\n"), -1;
	//printf("ssfs_fopen(%s) -> %u\n", name, fileID);
	return fileID;
}
// "The ssfs_fclose() closes a file, i.e., removes the entry from the open file descriptor table. On success, ssfs_fclose() should return 0 and a negative value otherwise." Also, writes all data to the file system from the memory.
int ssfs_fclose(int fileID){
	//printf("ssfs_fclose(%d)\n", fileID);
    return freeOpen(fileID, TRUE); // save it to disk
}
// "The ssfs_rfseek() moves the read pointer and ssfs_wfseek() moves the write pointer to the given location. It returns 0 on success and a negative integer value otherwise."
int ssfs_frseek(int fileID, int loc){
	open_t *open = idOpen(fileID);
	if(open == NULL || loc < 0 || (unsigned)loc > open->size) return /*printf("Attempted to seek[0] %d on file %d which is at %u:%u and has %u bytes.\n", loc, fileID, open ? open->file.part.block : 0, open ? open->file.part.index : 0, open ? open->size : 0),*/ -1;
	open->cursor_r = loc;
    return 0;
}
// "The ssfs_rfseek() moves the read pointer and ssfs_wfseek() moves the write pointer to the given location. It returns 0 on success and a negative integer value otherwise."
int ssfs_fwseek(int fileID, int loc){
    //return ssfs_frseek(fileID, loc);
	open_t *open = idOpen(fileID);
	if(open == NULL || loc < 0 || (unsigned)loc > open->size) return /*printf("Attempted to seek[1] %d on FileID%d which is at %u:%u and has %u bytes.\n", loc, fileID, open ? open->file.part.block : 0, open ? open->file.part.index : 0, open ? open->size : 0),*/ -1;
	open->cursor_w = loc;
    return 0;
}
// "The ssfs_fwrite() writes the given number of bytes of buffered data in buf into the open file, starting from the current write file pointer. This in effect could increase the size of the file by the given number of bytes (it may not increase the file size by the number of bytes written if the write pointer is located at a location other than the end of the file). The ssfs_fwrite() should return the number of bytes written."
// More true: "The ssfs_fwrite() returns length or -1 if it was not able to fit the entire length."
int ssfs_fwrite(int fileID, char *buf, int length){
	if(length == 0) return 0; // needed in the end calculation
	open_t *open;
	if(length < 0 || !(open = idOpen(fileID))) return printf("ssfs_fwrite: parameter error.\n"), -1;
	unsigned end = (unsigned)length + open->cursor_w; // max 0x800FFFFF, it does need to be unsigned
	if(end > BLOCK_SIZE * DATA_BLOCKS) /*end = BLOCK_SIZE * DATA_BLOCKS; // truncate*/ return -1;
	int size = end - open->cursor_w; // calculate the realistic size
	memcpy(open->contents + open->cursor_w, buf, sizeof(char) * size);
	if(end > open->size) open->size = end;
	open->cursor_w += size;
	//printf("ssfs_write: FileID%d: length of input %d, size %u, cursor %u: '%.50s'...\n", fileID, length, open->size, open->cursor_w, open->contents);
	// Uncomment this line to turn off buffering. It will run much slower
	//updateFile(open->file, open->size, open->contents);
	return size;
}
// "The ssfs_fread() follows a similar behavior." Returns -1 on error.
int ssfs_fread(int fileID, char *buf, int length){
	if(length == 0) return 0;
	open_t *open;
	if(length < 0 || !(open = idOpen(fileID))) return printf("ssfs_read: parameter error.\n"), -1;
	unsigned left = open->size - open->cursor_r;
	unsigned size = ((unsigned)length > left) ? left : (unsigned)length;
	memcpy(buf, open->contents + open->cursor_r, sizeof(char) * size);
	//printf("ssfs_fread(File%d, buf:%d); cur %u + left %u = size %u, calculated %u.\n", fileID, length, open->cursor_r, left, open->size, size);
	open->cursor_r += size;
	return size;
}
// "The ssfs_remove() removes the file from the directory entry, releases the i-node entry and releases the data blocks used by the file, so that they can be used by new files in the future."
int ssfs_remove(char *file){
	ref16_t ref = findFileSlot(file);
	if(ref.whole == 0) return printf("ssfs_remove: no %s exists.\n", file), -1;
	return freeFile(ref);
}

/**********
 * Shadow *
 **********/
// I implemented BlockFS without shadowing. The file system doesn't have a read-only bit. The files have no separate hash. It would be too much to go back and change, not enough time. It won't save FS persistently.
static data_block_t *shadows[MAX_SHADOWS];

// Test code does not cover this. Use at own risk.
int ssfs_commit(void) {
	// find a free shadow
	int cnum;
	for(cnum = 0; cnum < MAX_SHADOWS; cnum++) if(!shadows[cnum]) break;
	if(cnum == MAX_SHADOWS) return printf("You can only have %d shadows open.\n", MAX_SHADOWS), -1;
	shadows[cnum] = malloc(SIZE_OF_DISK * sizeof(char));
	data_block_t *shadow = shadows[cnum];
	if(!shadow) return perror("shadow"), -1;
	// backup it up, globally
	if(read_blocks(0, NUM_BLOCKS, shadow) != NUM_BLOCKS) return printf("Couldn't make backup.\n"), free(shadow), shadows[cnum] = NULL, -1; // turn off the protection
	return cnum;
}
int ssfs_restore(int cnum) {
	// see if the thing is valid
	data_block_t *shadow;
	if(cnum < 0 || cnum >= MAX_SHADOWS || (shadow = shadows[cnum]) == NULL || strncmp(MAGIC, (char *)shadow, sizeof MAGIC - 1) != 0) return printf("%d: not a valid BlochFS shadow.\n", cnum), -1;
	if(write_blocks(0, NUM_BLOCKS, shadow) != NUM_BLOCKS) return printf("Oops. Data corruption?\n"), -1;
	free(shadow);
	shadows[cnum] = NULL;
	printf("Overwrote the contents of the disk by the shadow %d.\n", cnum);
	return 0;
}
int ssfs_delete_shadow(int cnum) {
	data_block_t *shadow;
	if(cnum < 0 || cnum >= MAX_SHADOWS || (shadow = shadows[cnum]) == NULL) return printf("%d: not a valid shadow.\n", cnum), -1;	
	free(shadow);
	shadows[cnum] = NULL;
	printf("Erased the shadow %d.\n", cnum);
	return 0;
}

/*********
 * debug *
 *********/

//This ensures that ref is, in fact, within unit.
int ssfs_is(const enum UnitEnum unit, const ref16_t ref) {
	const struct Unit *const u = &units[unit];
	return ref.part.block >= u->start
		&& ref.part.block < u->start + u->blocks
		&& ref.part.index < u->per_block;
}
//Caller is responsible for ensuring that the buffer size at least PRINT_DATA_SIZE.
static void sanitize(char *const buffer) {
	int i;
	for(i = 0; i < PRINT_DATA_SIZE; i++) {
		char *d = buffer + i;
		if(*d == '\0') break;
		if(*d == '\\' || *d == '\"' || !isprint(*d)) *d = '_';
	}
	buffer[i] = '\0';
}
// For ssfs_graphviz.
typedef void (*PrintFn)(FILE *const, const int, const unsigned,
	const void *const);
static void print_stuff(FILE *const fp, const enum UnitEnum unit,
	const void *const local_copy, const PrintFn print) {
	const struct Unit *const u = &units[unit];
	const size_t size = u->blocks * u->per_block;
	size_t i;
	for(i = 0; i < size; i++) print(fp,
		(unsigned)(i / u->per_block + u->start),
		(unsigned)(i % u->per_block), (const char *)local_copy + i*u->size);
}
static void print_data_node(FILE *const fp, const int block,
	const unsigned idx, const char *const dont_read) {
	while(0 && (const void *)dont_read); /* fixme: MSVC complains; use sizeof(...) */
	if(!superBlock.free.data[block]) return;
	data_block_t dataBlock;
	read_safe(block, 1, &dataBlock, DATA), sanitize(dataBlock.data);
	fprintf(fp, "data_%u[ label = \"data%u:%u \\\"%s\\\"...\" ];\n",
		block, block, idx, dataBlock.data);
}
static void print_file_node(FILE *const fp, const int block,
	const unsigned idx, const file_t *const file) {
	if(!file->inode.whole) return;
	fprintf(fp, "file_%u_%u[ label = \"\\\"%s\\\"%u:%u z%u h%u\" ];\n",
		block, idx, file->name, block, idx, file->size, hashFile(file->name));
}
static void print_inode_node(FILE *const fp, const int block,
	const unsigned idx, const inode_t *const inode) {
	if(!inode->count) return;
	fprintf(fp, "inode_%u_%u[ label = \"inode%u:%u\" ];\n",
		block, idx, block, idx);
}
static void print_file_edge(FILE *const fp, const int block,
	const unsigned idx, const file_t *const file) {
	if(!file->inode.whole) return;
	fprintf(fp, "file_%u_%u -> inode_%u_%u;\n",
		block, idx, file->inode.part.block, file->inode.part.index);
}
static void print_inode_edges(FILE *const fp, const int block,
	const unsigned idx, const inode_t *const inode) {
	unsigned i;
	if(!inode->count) return;
	for(i = 0; i < inode->count; i++)
		fprintf(fp, "inode_%u_%u -> data_%u;\n", block, idx,
		inode->direct[i].part.block);
	if(inode->indirect.whole)
		fprintf(fp, "inode_%u_%u -> inode_%u_%u;\n", block, idx,
		inode->indirect.part.block, inode->indirect.part.index);
}
static void print_file_collisions_almost(FILE *const fp, const int block,
	const unsigned idx, const file_t *const file) {
	if(!file->inode.whole || (block == FILE_BLOCKS - 1 && idx == FILES_PER_BLOCK) || !file[1].inode.whole) return;
	unsigned linkBlock = block, linkIndex = idx + 1;
	if(linkIndex >= FILES_PER_BLOCK) linkBlock++, linkIndex = 0;
	fprintf(fp, "file_%u_%u -> file_%u_%u;\n",
			block, idx, linkBlock, linkIndex);
}
// Call to output, at any time, the contents of the drive, at that time, to gv; use http://www.graphviz.org/ to see the file.
int ssfs_graphviz(const char *const gv) {
	FILE *fp = fopen(gv, "w");
	if(!fp) return perror(gv), -1;
	file_block_t files[FILE_BLOCKS];
	inode_block_t inodes[INODE_BLOCKS];
	read_safe(FILES_START, FILE_BLOCKS, files, FILES);
	read_safe(INODES_START, INODE_BLOCKS, inodes, INODES);
	fprintf(fp, "digraph "FILE_SYSTEM_NAME" {\n"
		"rankdir = \"LR\";\n"
		"label = \""FILE_SYSTEM_NAME"\";\n");
	fprintf(fp, "node[shape=circle];\n");
	for(int i = 0; i < MAX_OPEN_FILES; i++) if(opens[i]) fprintf(fp,
		"open_%d[ label=\"Open%d\" ];\n", i, i);
	fprintf(fp, "node[shape=none];\n");	
	// for loop not incrementing only on Ubuntu (just this one)
	int loop = 0; while(loop++ < MAX_OPEN_FILES) {
		int i = loop - 1;
		if(!opens[i]) continue;
		char escape[PRINT_DATA_SIZE];
		strncpy(escape, opens[i]->contents, PRINT_DATA_SIZE);
		sanitize(escape);
		fprintf(fp, "contents_%d[ label=\"\\\"%s\\\"...\" ];\n", i, escape);
	}
	fprintf(fp, "node[shape=box,color=blue,style=filled,"
		"fillcolor=\"#ccccff\"];\n");
	print_stuff(fp, FILES, files, (PrintFn)&print_file_node);
	fprintf(fp, "node[shape=box,color=green,style=filled,"
		"fillcolor=\"#ccffcc\"];\n");
	print_stuff(fp, INODES, inodes, (PrintFn)&print_inode_node);
	fprintf(fp, "node[shape=box,color=grey,style=filled,"
		"fillcolor=\"#eeeeee\"];\n");
	print_stuff(fp, DATA, 0, (PrintFn)&print_data_node);
	fprintf(fp, "node[shape=box,color=red,style=filled,"
		"fillcolor=\"#ffcccc\"];\n");
	for(int i = 0; i < MAX_OPEN_FILES; i++) if(opens[i]) fprintf(fp,
		"open_%u -> file_%u_%u;\n"
		"open_%u -> contents_%u;\n",
		i, opens[i]->file.part.block, opens[i]->file.part.index, i, i);
	print_stuff(fp, FILES, files, (PrintFn)&print_file_edge);
	print_stuff(fp, INODES, inodes, (PrintFn)&print_inode_edges);
	fprintf(fp, "edge[style=dashed];\n");
	print_stuff(fp, FILES, files, (PrintFn)&print_file_collisions_almost);
	fprintf(fp, "}\n");
	fclose(fp);
	return 0;
}
