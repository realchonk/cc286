int global;

main ()
{
	char c, (c2);
	short s;
	int i;
	long l;

	int *x, f ();

	return c + s + i + l;
	return main;
	return &main;
	return *x;
}

tern ()
{
	short *x, a, b;

	return *x ? a : b;
}

ptradd ()
{
	int *p, x;
	return p + x;
}

ptrsub ()
{
	int *p1, *p2;
	int x;
	return p1 - p2;
	return p1 - x;
}

int
get_global ()
{
	return global;
}

// the following are examples of cursed things this compiler allows:

int var, func () {
	return var;
}


labels ()
{
	goto lbl;
	return lbl:, 0;
}


enum {
	A,
	B,
} x;

enums ()
{
	return A;
	return B;
	return x;
}
