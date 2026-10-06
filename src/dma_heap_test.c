// SPDX-License-Identifier: GPL-2.0
/*
 * dma_heap_test - allocate a buffer from a DMA-BUF heap, mmap it, write a
 * pattern, read it back and verify.
 *
 * Usage:
 *   ./dma_heap_test [heap_name] [size_bytes]
 *
 * Defaults: heap_name = "system", size = 1 MiB.
 *
 * Common heap names (see /dev/dma_heap/):
 *   system              - always present, cacheable, not physically contiguous
 *   default_cma_region  - CMA heap: cacheable + physically contiguous (use this
 *                         for a coherent/ACP DMA buffer)
 */

#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <linux/dma-heap.h>
#include <linux/dma-buf.h>

#define DEFAULT_HEAP "system"
#define DEFAULT_SIZE (1UL << 20)	/* 1 MiB */

/* Bracket CPU access so the heap can do any needed cache maintenance. */
static int dmabuf_sync(int fd, uint64_t flags)
{
	struct dma_buf_sync sync = { .flags = flags };
	int ret;

	do {
		ret = ioctl(fd, DMA_BUF_IOCTL_SYNC, &sync);
	} while (ret == -1 && (errno == EINTR || errno == EAGAIN));

	return ret;
}

int main(int argc, char **argv)
{
	const char *heap_name = (argc > 1) ? argv[1] : DEFAULT_HEAP;
	size_t size = (argc > 2) ? strtoul(argv[2], NULL, 0) : DEFAULT_SIZE;
	char heap_path[256];
	int heap_fd, dmabuf_fd, ret = EXIT_FAILURE;
	struct dma_heap_allocation_data alloc = { 0 };
	uint8_t *buf;
	size_t i;

	snprintf(heap_path, sizeof(heap_path), "/dev/dma_heap/%s", heap_name);

	heap_fd = open(heap_path, O_RDWR | O_CLOEXEC);
	if (heap_fd < 0) {
		fprintf(stderr, "open(%s): %s\n", heap_path, strerror(errno));
		fprintf(stderr, "Available heaps:\n");
		ret = system("ls /dev/dma_heap/ 2>/dev/null");	/* best effort */
		(void)ret;
		return EXIT_FAILURE;
	}

	/* 1. Allocate a dma-buf of the requested size from the heap. */
	alloc.len = size;
	alloc.fd_flags = O_RDWR | O_CLOEXEC;
	if (ioctl(heap_fd, DMA_HEAP_IOCTL_ALLOC, &alloc) < 0) {
		fprintf(stderr, "DMA_HEAP_IOCTL_ALLOC: %s\n", strerror(errno));
		goto out_heap;
	}
	dmabuf_fd = alloc.fd;
	printf("Allocated %zu bytes from heap '%s' (dmabuf fd=%d)\n",
	       size, heap_name, dmabuf_fd);

	/* 2. Map it into our address space. */
	buf = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, dmabuf_fd, 0);
	if (buf == MAP_FAILED) {
		fprintf(stderr, "mmap: %s\n", strerror(errno));
		goto out_dmabuf;
	}

	/* 3. Write a test pattern. */
	dmabuf_sync(dmabuf_fd, DMA_BUF_SYNC_START | DMA_BUF_SYNC_WRITE);
	for (i = 0; i < size; i++)
		buf[i] = (uint8_t)(i * 7 + 0x5a);
	dmabuf_sync(dmabuf_fd, DMA_BUF_SYNC_END | DMA_BUF_SYNC_WRITE);
	printf("Wrote test pattern.\n");

	/* 4. Read it back and verify. */
	dmabuf_sync(dmabuf_fd, DMA_BUF_SYNC_START | DMA_BUF_SYNC_READ);
	for (i = 0; i < size; i++) {
		uint8_t expect = (uint8_t)(i * 7 + 0x5a);

		if (buf[i] != expect) {
			fprintf(stderr,
				"Mismatch at %zu: got 0x%02x, expected 0x%02x\n",
				i, buf[i], expect);
			dmabuf_sync(dmabuf_fd,
				    DMA_BUF_SYNC_END | DMA_BUF_SYNC_READ);
			goto out_unmap;
		}
	}
	dmabuf_sync(dmabuf_fd, DMA_BUF_SYNC_END | DMA_BUF_SYNC_READ);
	printf("Read back and verified %zu bytes: OK\n", size);

	ret = EXIT_SUCCESS;

out_unmap:
	munmap(buf, size);
out_dmabuf:
	close(dmabuf_fd);
out_heap:
	close(heap_fd);
	return ret;
}