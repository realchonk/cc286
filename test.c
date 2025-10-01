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
};

enum log_level {
	DEBUG,
	INFO,
	ERROR,
};

enums ()
{
	enum log_level lvl;

	return A;
	return B;
	return lvl;
}

struct stat {
	unsigned short	st_mode;
	unsigned long	st_size;
};

struct file {
	char		*f_name;
	struct stat	*f_st;
};

structs ()
{
	union {
		int x;
		long z;
	} u, *u_ptr;
	struct stat st, *st_ptr;
	struct file *f;

	return st.st_mode;
	return &st.st_size;
	return u.x + u_ptr->x;
	return f->f_st->st_mode;
}

// recurse ()
// {
// 	return recurse ();
// }

write ()
{
	int x, z;
	x = z = 1;
	return x;
}
