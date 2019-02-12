#include <stdint.h>
#include <stdio.h>

struct RGBImage
{
	uint8_t * data;
	uint32_t width;
	uint32_t height;
};
int main()
{
	struct RGBImage rgb;
	printf("%ld %ld %ld %ld\n", (long)&rgb.data-(long)&rgb, (long)&rgb.width-(long)&rgb, (long)&rgb.height-(long)&rgb, sizeof rgb);
	return 0;
}

