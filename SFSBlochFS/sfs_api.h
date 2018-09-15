#define FALSE 0
#define TRUE 1

// Declare constants. don't change these
#define BLOCK_SIZE 1024
#define NUM_BLOCKS 1024
#define SIZE_OF_DISK (NUM_BLOCKS * BLOCK_SIZE)
#define MAX_FILENAME 11
#define INODE_DIRECTS 14
#define MAGIC "\xAC\xBD\x00\x05""BlochFS"

// You can change these
#define MAX_OPEN_FILES 200
#define FILE_SYSTEM_NAME "myFile"
#define PRINT_DATA_SIZE 10
#define MAX_SHADOWS 64

// the different units
// b = BLOCK_SIZE
// a = NUM_BLOCKS
// s = 1 superblock-free-bitmap
// d = 31 directory blocks > ^z/64^, more will work faster
// n = 31 inode blocks, ^z/32^, more will be wasted
// z = a-s-d-n = 961 data blocks
//
// z/64d = 52% maximum load factor
// 1-z/a = 6% waste

#define SUPER_START 0
#define SUPER_BLOCKS 1

#define FILES_START (SUPER_START + SUPER_BLOCKS)
#define FILES_PER_BLOCK 64
#define FILE_BLOCKS 31

#define INODES_START (FILES_START + FILE_BLOCKS)
#define INODES_PER_BLOCK 32
#define INODE_BLOCKS 31

#define DATA_START (INODES_START + INODE_BLOCKS)
#define DATA_BLOCKS (NUM_BLOCKS - SUPER_BLOCKS - INODE_BLOCKS - FILE_BLOCKS)






#include <stdint.h> // uint16_t

// references to the file system, index is context dependant, depending on the unit, size = 2
// ref.whole = 0 is null
// http://stackoverflow.com/questions/252552/why-do-we-need-c-unions
typedef union
{
	uint16_t whole;
	// GNU extension: it very well could work on other compilers?
	struct
	{
		uint16_t block : 10; // 1024
		uint16_t index : 6; // 64
	} part;
} ref16_t;

// 1024 bytes of data
typedef struct { char data[BLOCK_SIZE]; } data_block_t;

// inode structure, size = 32, 32 * 32 = 1024
typedef struct
{
	uint16_t count; // size > 0, else null
	ref16_t direct[INODE_DIRECTS]; // ref to data
	ref16_t indirect; // ref to inode
} inode_t;
typedef struct { inode_t inodes[INODES_PER_BLOCK]; } inode_block_t;

// directory structure: associates filenames to inodes size = 16, 16 * 64 = 1024
// the hash and the inode are not null, if they are, the entry is null
// dir_t was confusing to refer to a directory entry, renamed file_t
typedef struct
{
	ref16_t inode; // 0 is null
	uint16_t size;
	char name[MAX_FILENAME + 1];
} file_t;
typedef struct { file_t files[FILES_PER_BLOCK]; } file_block_t;

// The first block inside the file system, sizeof(super_t) < START_DATA_BLOCK
typedef struct
{
	char magic[60];
	// int number_of_inodes; <- changed design to random sampling, don't need <- don't need random sampling, inodes are a subset of blocks
	// int number_of_directory; <- changed design to hashing, don't need
} super_t;
typedef union { super_t super; data_block_t free; } super_block_t;




// print/debug/whatever
static const struct Unit {
	const char *const name;
	const size_t size, start, blocks, per_block;
} units[] = {
	{ "super", 0, SUPER_START, SUPER_BLOCKS, 1 },
	{ "directory", sizeof(file_t), FILES_START, FILE_BLOCKS, FILES_PER_BLOCK },
	{ "inodes", sizeof(inode_t), INODES_START, INODE_BLOCKS, INODES_PER_BLOCK },
	{ "data", 0, DATA_START, DATA_BLOCKS, 1 }
};
enum UnitEnum { SUPER, FILES, INODES, DATA, NONE };

//Functions you should implement. 
//Return -1 for error besides mkssfs
void mkssfs(int fresh);
int ssfs_fopen(char *name);
int ssfs_fclose(int fileID);
int ssfs_frseek(int fileID, int loc);
int ssfs_fwseek(int fileID, int loc);
int ssfs_fwrite(int fileID, char *buf, int length);
int ssfs_fread(int fileID, char *buf, int length);
int ssfs_remove(char *file);
int ssfs_commit(void);
int ssfs_restore(int cnum);
// added this
int ssfs_delete_shadow(int cnum);
// added debug functions
int ssfs_is(const enum UnitEnum unit, const ref16_t ref);
int ssfs_graphviz(const char *const fn);
