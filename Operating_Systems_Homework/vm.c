/**
* Memory manager.
*
* Uses a TLB with FIFO replacement
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

// number of characters to read for each line from input file
#define BUFFER_SIZE   10

// number of bytes to read
#define CHUNK   256

// mask for the offset
#define BIT_MASK 0xff

// mask for the address
#define ADDRESS_MASK 0xFFFF

#define PAGE_TABLE_SIZE   256
#define TLB_SIZE    16

FILE    *address_file;
FILE    *backing_store;

// how we store reads from input file
char    address[BUFFER_SIZE];
int     logical_address;

// the buffer containing reads from backing store
signed char     buffer[CHUNK];

// the value of the byte (signed char) in memory
signed char     value;

// next available page table entry
int available_page_table_number = 0;

// next available TLB entry
int available_tlb_entry = 0;

// tracks the least recent entry to TLB (for replacement)
int earliest_tlb_entry = 0;

// stat tracking
int page_faults = 0;
int translated_addresses = 0;
int tlb_hits = 0;

// representing page table entries
struct P_T_E {
  int frame;
  int valid;

// to be cool, make one variable an int (which is 32 bits). 8 bits for 'frame', 1 for 'valid'
// use bit masking to access 'frame' and 'valid'
// frame mask would be 0xff
// valid mask would be 0X100
}page_table[PAGE_TABLE_SIZE];

// 2-D array for physical memory
struct Frame {
  char physical_memory[CHUNK][CHUNK];
  int next_available_frame;
}physical_frame;

// representing the TLB
struct TLB {
  int page_number;
  int frame_number;
}tlb[TLB_SIZE];

// function prototypes
void get_page(int logical_address);
void read_from_backing_store(int page_number);
int in_tlb(int page_number);
void insert_into_tlb(int page_number, int frame_number);


void get_page(int logical_address) {

// retrieve the page_number and offset from the logical address
int page_number = (logical_address >> 8) & BIT_MASK;
int offset = logical_address & BIT_MASK;

// returns the index of the page number in the TLB, or -2 if not present
int frame_number = in_tlb(page_number);

// if page_number is not in the TLB, check page_table
if (frame_number == -2) {

  // page_number is present in page_table
  if (page_table[page_number].valid == 1) {
    frame_number = page_table[page_number].frame;

    if (fseek(backing_store, (page_number * CHUNK), SEEK_SET) != 0) {
      fprintf(stderr, "Error seeking in backing store\n");
    }

    // now read CHUNK bytes from the backing store to the buffer
    if (fread(&physical_frame.physical_memory[frame_number], sizeof(signed char), CHUNK, backing_store) == 0) {
      fprintf(stderr, "Error reading from backing store\n");
    }
  }

  // page_number not in page_table, so go to backing store and get it
  else {
    // reads in the data from the backing store
    read_from_backing_store(page_number);
    page_faults++;
    frame_number = page_table[page_number].frame;
  }

  // inserts this new page table entry into the TLB
  insert_into_tlb(page_number, frame_number);
}

value = physical_frame.physical_memory[frame_number][offset];
printf("virtual address: %d, physical address: %i, value: %i\n", logical_address, (frame_number << 8) | offset, value);
}

void read_from_backing_store(int page_number) {
if (fseek(backing_store, (page_number * CHUNK), SEEK_SET) != 0) {
  fprintf(stderr, "Error seeking in backing store\n");
}

// now read CHUNK bytes from the backing store to the buffer
if (fread(&physical_frame.physical_memory[page_table[page_number].frame], sizeof(signed char), CHUNK, backing_store) == 0) {
  fprintf(stderr, "Error reading from backing store\n");
}

// sets the page_number and frame_number for P_T_E
page_table[available_page_table_number].frame = page_number;
physical_frame.physical_memory[available_page_table_number][0] = physical_frame.next_available_frame;

// set the valid bit for the P_T_E
page_table[page_number].valid = 1;

// incrementing the next available page table entry/frame
available_page_table_number++;
physical_frame.next_available_frame++;
}

/**
* Checks the TLB for the current page_table
* Returns the index of the page_table
* or -2 if not present
*/
int in_tlb(int page_number) {
for (int i = 0; i < TLB_SIZE; i++) {
  if (tlb[i].page_number == page_number) {
    printf("tlb[%i].frame_number: %i\n", i, tlb[i].frame_number);
    tlb_hits++;
    return i;
  }
}
return -2;
}

/*
* Inserts the current page table entry into the TLB
* Only called when something is not in TLB
*/
void insert_into_tlb(int page_number, int frame_number) {

// if TLB is not full
if (available_tlb_entry < TLB_SIZE) {
  tlb[available_tlb_entry].page_number = page_number;
  tlb[available_tlb_entry].frame_number =  frame_number;
  available_tlb_entry++;
}

// TLB full, use FIFO to replace entries
else {
  tlb[earliest_tlb_entry].page_number = page_number;
  tlb[earliest_tlb_entry].frame_number = frame_number;

  // checking whether to reset earliest_tlb_entry to 0, or just increment
  if (earliest_tlb_entry < TLB_SIZE - 1) {
    earliest_tlb_entry++;
  }
  else {
  earliest_tlb_entry = 0;
  }
}
}


int main(int argc, char *argv[]) {

// perform basic error checking
if (argc != 3) {
  fprintf(stderr,"Usage: ./vm [backing store] [input file]\n");
  return -1;
}

// open the file containing the backing store
backing_store = fopen(argv[1], "rb");

if (backing_store == NULL) {
  fprintf(stderr, "Error opening %s\n",argv[1]);
  return -1;
}

// open the file containing the logical addresses
address_file = fopen(argv[2], "r");

if (address_file == NULL) {
  fprintf(stderr, "Error opening %s\n",argv[2]);
  return -1;
}

// struct Frame physical_frame;
physical_frame.next_available_frame = 0;

// default struct for a p_t_e entry
struct P_T_E entry;
entry.frame = 0;
entry.valid = 0;

// default struct for a TLB entry
struct TLB tlb_temp;
tlb_temp.page_number = -1;
tlb_temp.frame_number = -1;

// initialize page_table with all the p_t_e's and TLB with holder TLB entries
for (int i = 0; i < PAGE_TABLE_SIZE; i++) {
  page_table[i] = entry;

  if (i < TLB_SIZE) {
    tlb[i] = tlb_temp;
  }
}

// read through the input file and output each logical address
while (fgets(address, BUFFER_SIZE, address_file) != NULL) {
  logical_address = atoi(address);

  // body of the program.
  get_page(logical_address);
  translated_addresses++;
}

double page_fault_rate = page_faults / (double)translated_addresses;
double tlb_rate = tlb_hits / (double)translated_addresses;

printf("Number of translated addresses: %i\n", translated_addresses);
printf("Number of page faults: %i\n", page_faults);
printf("Page fault rate: %.3f\n", page_fault_rate);
printf("Number of TLB hits: %i\n", tlb_hits);
printf("TLB hit rate: %.3f\n", tlb_rate);

fclose(address_file);
fclose(backing_store);

return 0;
}
