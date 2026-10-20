// Track explicit stack slots and owned buffer allocations, including failures.
#define _DEFAULT_SOURCE
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#ifndef FAIL_STACK
#define FAIL_STACK 0
#endif
#ifndef FAIL_BUFFER
#define FAIL_BUFFER 0
#endif
static unsigned stacks, stack_frees, buffers, buffer_frees, units, unit_frees;
static unsigned stack_attempts, buffer_attempts, failures;
typedef union {
    max_align_t alignment;
    struct { size_t extent, size; } info;
} Header;
void *malloc(size_t size) {
    if (size == 558 && ++stack_attempts == FAIL_STACK) { failures++; return NULL; }
    if (size == 8192 && ++buffer_attempts == FAIL_BUFFER) { failures++; return NULL; }
    if (size > SIZE_MAX - sizeof(Header)) return NULL;
    size_t extent = sizeof(Header) + size;
    Header *header = mmap(NULL, extent, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (header == MAP_FAILED) return NULL;
    header->info.extent = extent;
    header->info.size = size;
    stacks += size == 558;
    buffers += size == 8192;
    units += size == 1;
    memset(header + 1, 0xa5, size);
    return header + 1;
}
void free(void *pointer) {
    if (!pointer) return;
    Header *header = (Header *)pointer - 1;
    stack_frees += header->info.size == 558;
    buffer_frees += header->info.size == 8192;
    unit_frees += header->info.size == 1;
    munmap(header, header->info.extent);
}
__attribute__((destructor)) static void check_releases(void) {
    unsigned expected_stack = FAIL_STACK ? 0 : 1;
    unsigned expected_buffers = FAIL_STACK ? 0 : (FAIL_BUFFER ? FAIL_BUFFER - 1 : 4);
    unsigned expected_attempts = FAIL_STACK ? 0 : (FAIL_BUFFER ? FAIL_BUFFER : 4);
    unsigned expected_failures = (FAIL_STACK != 0 || FAIL_BUFFER != 0) ? 1 : 0;
    fprintf(stderr, "stacks=%u/%u buffers=%u/%u units=%u/%u attempts=%u/%u failures=%u\n",
            stacks, stack_frees, buffers, buffer_frees, units, unit_frees,
            stack_attempts, buffer_attempts, failures);
    if (stacks != expected_stack || stack_frees != expected_stack ||
        buffers != expected_buffers || buffer_frees != expected_buffers ||
        units != expected_stack || unit_frees != expected_stack ||
        stack_attempts != 1 || buffer_attempts != expected_attempts ||
        failures != expected_failures) _Exit(101);
}
