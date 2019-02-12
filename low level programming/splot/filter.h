#ifndef __FILTER_H__
#define __FILTER_H__

#include <stdint.h>

struct RGBImage
{
	uint8_t * data;
	uint32_t width;
	uint32_t height;
};

void filter(struct RGBImage * dst, const struct RGBImage * src, const float * matrix);

#endif
