#include <stdint.h>
#include <stdio.h>

#include "ptlcalls.h"

struct Data
{
	uint64_t len;
	uint64_t * limbs;
} data;

extern void ctor(struct Data *, const char *);

int main()
{
	char buffer[65536];
	int i;
	scanf("%s", buffer);
	data.limbs = (uint64_t*) malloc(32000); // enough
	data.len = 0;

	ptlcall_switch_to_sim();
	ctor(&data, "");
	ptlcall_switch_to_native();
	
	printf("%lu", data.len);
	
	free(data.limbs);
	return 0;
}
