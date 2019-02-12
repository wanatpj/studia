#include<cstdio>
#include "D.hpp"
int main(){
	Natural n,n2;
	n += n2;
	n2.Print();
	Natural m(n); n.Print();
	Natural b(3); b.Print();
	n.Print();
	Natural q1("000000000000000000001234567890987654321abcdefedcba00000000000000902384093829483029840398209483092840382094830928490832098432090000001234567890987654321abcdefedcba000000000000000000001234567890987654321abcdefedcba000000000000000000001234567890987654321abcdefedcba000000000000000000001234567890987654321abcdefedcba000000000000000000001234567890987654321abcdefedcba000000000000000000001234567890987654321abcdefedcba000000000000000000001234567890987654321abcdefedcba000000000000000000001234567890987654321abcdefedcba000000000000000000001234567890987654321abcdefedcba000000000000000000001234567890987654321abcdefedcba");
	b += q1;
	printf("\n");
	b.Print();
	printf("\n");
	Natural f("ffffffffffffffff");
	f.Print(); printf("\n");
	f += f;
	f.Print(); printf("\n");
	f = Natural("ffffffffffffffff");
	f *= Natural();
	f = Natural();
	f *= Natural("ffffffffffffffffffffffffffffffff");
	f.Print();
	printf("\n");
	Natural w("e");
	q1.Print();
	w.Print();
	Natural qq1(q1);
	qq1.Print();
	n = qq1;
	n.Print();
	Natural qqq = Natural(Natural(Natural(n)));
	printf("\n");
	qqq.Print();
	n = Natural(qqq);
	printf("\n");
	n.Print();
	
	printf("\n");
	b.Print(); printf("\n");
	printf("b.Size(): %lu\n", b.Size());
	
	
	Natural c(0x02020202); c.Print();
	printf("c.Size(): %lu\n", c.Size());
	Natural d(b); d.Print();
	n = m = b;
	n = n;
	c += c;
	printf("c.Size(): %lu\n", c.Size());
	c.Print();
	Natural q("ffffffffffffffff");
	q.Print();
	Natural r("1234549379843000");
	r.Print();
	r *= b;
	r.Print();
	r = Natural("1234549379843000");
	r.Print();
	r *= q;
	r.Print();
	printf("\n");
	b.Print(); printf("\n");
	printf("b.Size(): %lu\n", b.Size());
	printf("\n");
	r.Print(); printf("\n");
	printf("r.Size(): %lu\n", r.Size());
	printf("c.Size(): %lu\n", c.Size());
	Natural qq("10000000000000000");
	Natural qq2("30000000000000000");
	printf("qq: "); qq.Print(); printf("\nqq2: "); qq2.Print(); printf("\nr: "); r.Print(); printf("\n");
	printf("\n%d %d\n", qq==r, r==r);
	printf("\nqq<r: %d\n", qq<r);
	printf("\nr<qq: %d\n", r<qq);
	printf("\nr<r: %d\n", r<r);
	printf("\nqq<qq2: %d qq2<qq: %d\n", qq<qq2, qq2<qq);
	return 0;
}

