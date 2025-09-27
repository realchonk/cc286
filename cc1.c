#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <err.h>

#define MAXREGS 256
#define MAXIDENT 8

#define new(T) ((T *)calloc (1, sizeof (T)))
#define copyident(dst, src) (memcpy ((dst), (src), MAXIDENT + 1))

enum codemodel {
	/* sizeof(ptr) = 2 */
	CM_SMALL,

	/* sizeof(ptr) = 4 */
	CM_LARGE,
} cm;


/* LEXER */

enum token_type {
	TOK_NULL,
	TOK_INT,
	TOK_IDENT,

	TOK_AMP,
	TOK_SEMI,
	TOK_PLUS,
	TOK_MINUS,
	TOK_STAR,
	TOK_SLASH,
	TOK_LPAR,
	TOK_RPAR,
	TOK_LCURLY,
	TOK_RCURLY,
	TOK_COMMA,
	TOK_QMARK,
	TOK_COLON,

	KW_RETURN,
	KW_SIGNED,
	KW_UNSIGNED,
	KW_CHAR,
	KW_SHORT,
	KW_INT,
	KW_LONG,

	TOK_EOF,
};

union {
	long i;
	char id[MAXIDENT + 1];
} lval;

static int
isident (ch)
{
	return isalnum (ch) || ch == '_';
}

enum token_type
lex ()
{
	int ch, i;

begin:	do {
		ch = getchar ();
	} while (isspace (ch));

	if (isdigit (ch)) {
		lval.i = 0;
		do {
			lval.i = lval.i * 10 + (ch - '0');
			ch = getchar ();
		} while (isdigit (ch));
		ungetc (ch, stdin);
		return TOK_INT;
	} else if (isident (ch)) {
		i = 0;
		do {
			if (i < MAXIDENT)
				lval.id[i++] = ch;
			ch = getchar ();
		} while (isident (ch));
		ungetc (ch, stdin);
		lval.id[i] = '\0';
		if (strcmp (lval.id, "return") == 0) {
			return KW_RETURN;
		} else if (strcmp (lval.id, "signed") == 0) {
			return KW_SIGNED;
		} else if (strcmp (lval.id, "unsigned") == 0) {
			return KW_UNSIGNED;
		} else if (strcmp (lval.id, "char") == 0) {
			return KW_CHAR;
		} else if (strcmp (lval.id, "short") == 0) {
			return KW_SHORT;
		} else if (strcmp (lval.id, "int") == 0) {
			return KW_INT;
		} else if (strcmp (lval.id, "long") == 0) {
			return KW_LONG;
		} else {
			return TOK_IDENT;
		}
	} else switch (ch) {
	case '&':
		return TOK_AMP;
	case '(':
		return TOK_LPAR;
	case ')':
		return TOK_RPAR;
	case '{':
		return TOK_LCURLY;
	case '}':
		return TOK_RCURLY;
	case ',':
		return TOK_COMMA;
	case ';':
		return TOK_SEMI;
	case '+':
		return TOK_PLUS;
	case '-':
		return TOK_MINUS;
	case '*':
		return TOK_STAR;
	case '/':
		ch = getchar ();
		switch (ch) {
		case '/':
			do {
				ch = getchar ();
			} while (ch != '\n');
			goto begin;
		case '*':
			while (1) {
				while (getchar () != '*');
				if (getchar () == '/')
					goto begin;
			}
		default:
			ungetc (ch, stdin);
			return TOK_SLASH;
		}
	case '?':
		return TOK_QMARK;
	case ':':
		return TOK_COLON;
	case EOF:
		return TOK_EOF;
	default:
		errx (1, "invalid input: '%c'", ch);
	}
}

static int peekd = TOK_NULL;

enum token_type
peek ()
{
	if (peekd == TOK_NULL)
		peekd = lex ();
	return peekd;
}

enum token_type
next ()
{
	int tok;

	if (peekd != TOK_NULL) {
		tok = peekd;
		peekd = TOK_NULL;
	} else {
		tok = lex ();
	}
	
	return tok;
}

void
expect (exp)
enum token_type exp;
{
	enum token_type tk;
	tk = next ();
	if (tk != exp)
		errx (1, "expected token %d, got %d", exp, tk);
}

int
match (exp)
enum token_type exp;
{
	return peek () == exp ? next (), 1 : 0;
}

/* TYPE SYSTEM */

enum dtype_type {
	DT_NULL,
	DT_LBL,
	DT_PTR,
	DT_FUNC,
	DT_CHAR,
	DT_SCHAR,
	DT_UCHAR,
	DT_SHORT,
	DT_USHORT,
	DT_INT,
	DT_UINT,
	DT_LONG,
	DT_ULONG,
};

struct dtype {
	enum dtype_type type;
	int refcnt;
	struct dtype *inner;
};


#define builtin(ty) { ty, 1, NULL }
static struct dtype dt_lbl	= builtin (DT_LBL);
static struct dtype dt_char	= builtin (DT_CHAR);
static struct dtype dt_schar	= builtin (DT_SCHAR);
static struct dtype dt_uchar	= builtin (DT_UCHAR);
static struct dtype dt_short	= builtin (DT_SHORT);
static struct dtype dt_ushort	= builtin (DT_USHORT);
static struct dtype dt_int	= builtin (DT_INT);
static struct dtype dt_uint	= builtin (DT_UINT);
static struct dtype dt_long	= builtin (DT_LONG);
static struct dtype dt_ulong	= builtin (DT_ULONG);

struct dtype *
copy_dt (dt)
struct dtype *dt;
{
	dt->refcnt++;
	return dt;
}

void
print_dt (dt)
struct dtype *dt;
{
	switch (dt->type) {
	case DT_NULL:
		abort ();
	case DT_LBL:
		printf ("label");
		break;
	case DT_PTR:
		printf ("ptr");
		break;
	case DT_FUNC:
		printf ("func");
		break;
	case DT_CHAR:
	case DT_SCHAR:
	case DT_UCHAR:
		printf ("byte");
		break;
	case DT_SHORT:
	case DT_USHORT:
	case DT_INT:
	case DT_UINT:
		printf ("word");
		break;
	case DT_LONG:
	case DT_ULONG:
		printf ("dword");
		break;
	}
}

void
free_dt (dt)
struct dtype *dt;
{
	--dt->refcnt;
	if (dt->refcnt != 0)
		return;

	switch (dt->type) {
	case DT_NULL:
		abort ();
	case DT_PTR:
	case DT_FUNC:
		free_dt (dt->inner);
		break;
	case DT_LBL:
	case DT_CHAR:
	case DT_SCHAR:
	case DT_UCHAR:
	case DT_SHORT:
	case DT_USHORT:
	case DT_INT:
	case DT_UINT:
	case DT_LONG:
	case DT_ULONG:
		break;
	}
	free (dt);
}

int
sizeof_dt (dt)
struct dtype *dt;
{
	switch (dt->type) {
	case DT_NULL:
		abort ();
	case DT_PTR:
		switch (cm) {
		case CM_SMALL:
			return 2;
		case CM_LARGE:
			return 4;
		}
	case DT_FUNC:
		switch (cm) {
		case CM_SMALL:
			return 2;
		case CM_LARGE:
			return 4;
		}
	case DT_LBL:
		// TODO: code models
		abort ();
	case DT_CHAR:
	case DT_SCHAR:
	case DT_UCHAR:
		return 1;
	case DT_SHORT:
	case DT_USHORT:
	case DT_INT:
	case DT_UINT:
		return 2;
	case DT_LONG:
	case DT_ULONG:
		return 4;
	}
}

struct dtype *
ptr_dt (inner, inc)
struct dtype *inner;
{
	struct dtype *ptr;

	ptr = new (struct dtype);
	ptr->type = DT_PTR;
	ptr->refcnt = 1;
	ptr->inner = inner;

	if (inc)
		++inner->refcnt;

	return ptr;
}

void
assert_dt_eq (dta, dtb)
struct dtype *dta, *dtb;
{
	assert (dta->type == dtb->type);

	switch (dta->type) {
	case DT_PTR:
	case DT_FUNC:
		return assert_dt_eq (dta->inner, dtb->inner);
	default:
		break;
	}
}

int
is_signed (dt)
struct dtype *dt;
{
	switch (dt->type) {
	case DT_CHAR:
	case DT_SCHAR:
	case DT_SHORT:
	case DT_INT:
	case DT_LONG:
		return 1;
	case DT_UCHAR:
	case DT_USHORT:
	case DT_UINT:
	case DT_ULONG:
		return 0;
	default:
		abort ();
	}
}

/* REGISTER SET */

static int reg = 0;
struct reg {
	struct dtype *r_dt;
	int r_is_lv;
};
static struct reg regs[MAXREGS];

void
reset_regs ()
{
	int i;
	for (i = 0; i < MAXREGS; ++i) {
		if (regs[i].r_dt != NULL) {
			free_dt (regs[i].r_dt);
			regs[i].r_dt = NULL;
		}
	}
	reg = 0;
}

int
alloc_reg (dt, lv)
struct dtype *dt;
{
	int r;
	r = reg++;
	regs[r].r_dt = dt;
	regs[r].r_is_lv = lv;
	return r;
}

/* SYMBOLS */

struct symbol {
	struct symbol *next;
	char id[MAXIDENT + 1];
	struct dtype *dt;
	int reg;
};

static struct symbol *fvars = NULL, *gvars = NULL;

struct symbol *
lookup_in (head, id)
struct symbol *head;
char *id;
{
	for (; head != NULL; head = head->next) {
		if (strcmp (head->id, id) == 0)
			return head;
	}
	return NULL;
}

struct symbol *
lookup (id)
char *id;
{
	struct symbol *sym;

	if ((sym = lookup_in (fvars, id)) != NULL)
		return sym;

	if ((sym = lookup_in (gvars, id)) != NULL)
		return sym;

	errx (1, "invalid symbol: %s", id);
}

/* PARSER */

int
rvalue (r)
{
	struct reg *reg;
	int x;

	reg = &regs[r];
	if (!reg->r_is_lv || reg->r_dt->type == DT_FUNC)
		return r;

	assert (reg->r_dt->type == DT_PTR);
	x = alloc_reg (copy_dt (reg->r_dt->inner), 0);
	printf ("\tlet $%d: ", x);
	print_dt (reg->r_dt->inner);
	printf (" = read $%d;\n", r);
	return x;
}

int
lvalue (r)
{
	if (!regs[r].r_is_lv)
		errx (1, "must be an lvalue");
	return r;
}

int expr ();

int
atom ()
{
	struct symbol *sym;
	int r;

	switch (next ()) {
	case TOK_INT:
		r = alloc_reg (copy_dt (&dt_int), 0);
		printf ("\tlet $%d: word = %ld;\n", r, lval.i);
		break;
	case TOK_IDENT:
		sym = lookup (lval.id);
		if (sym->reg != -1) {
			r = sym->reg;
		} else {
			r = alloc_reg (copy_dt (sym->dt), 1);
			printf ("\tlet $%d: ", r);
			print_dt (sym->dt);
			printf (" = load %s;\n", sym->id);
		}
		break;
	case TOK_LPAR:
		r = expr ();
		expect (TOK_RPAR);
		break;
	default:
		errx (1, "expected expression");
	}

	return r;
}

int
unary ()
{
	struct dtype *dt;
	int r, s;

	switch (peek ()) {
	case TOK_AMP:
		next ();
		s = lvalue (unary ());
		r = alloc_reg (copy_dt (regs[s].r_dt), 0);
		printf ("\tlet $%d: ", r);
		print_dt (regs[r].r_dt);
		printf (" = $%d;\n", s);
		break;
	case TOK_STAR:
		next ();
		s = rvalue (unary ());
		r = alloc_reg (copy_dt (regs[s].r_dt), 1);
		printf ("\tlet $%d: ", r);
		print_dt (regs[s].r_dt);
		printf (" = $%d;\n", s);
		break;
	case TOK_MINUS:
		next ();
		s = rvalue (unary ());
		dt = regs[s].r_dt;
		r = alloc_reg (copy_dt (dt), 0);
		printf ("\tlet $%d = -$%d;\n", r, s);
		break;
	default:
		r = atom ();
		break;
	}

	return r;
}

int
promote (a)
{
	struct dtype *adt;
	int r;

	adt = regs[a].r_dt;

	switch (adt->type) {
	case DT_NULL:
	case DT_LBL:
		abort ();
	case DT_CHAR:
	case DT_SCHAR:
		r =  alloc_reg (copy_dt (&dt_int), 0);
		printf ("\tlet $%d: word = sext $%d;\n", r, a);
		break;
	case DT_UCHAR:
		r = alloc_reg (copy_dt (&dt_uint), 0);
		printf ("\tlet $%d: word = zext $%d;\n", r, a);
		break;
	case DT_SHORT:
		r = alloc_reg (copy_dt(&dt_int), 0);
		printf ("\tlet $%d: word = $%d;\n", r, a);
		break;
	case DT_USHORT:
		r = alloc_reg (copy_dt(&dt_uint), 0);
		printf ("\tlet $%d: word = $%d;\n", r, a);
		break;
	case DT_PTR:
	case DT_FUNC:
	case DT_INT:
	case DT_UINT:
	case DT_LONG:
	case DT_ULONG:
		r = a;
		break;
	}
	return r;
}

void
promote2 (a, b)
int *a, *b;
{
	struct dtype *dta, *dtb;
	int t, u;

	*a = promote (*a);
	*b = promote (*b);

	dta = regs[*a].r_dt;
	dtb = regs[*b].r_dt;

	if (dta->type == dtb->type)
		return;

	if (dta->type == DT_ULONG) {
		t = alloc_reg (copy_dt (&dt_ulong), 0);
		printf ("\tlet $%d: dword = zext $%d;\n", t, *b);
		*b = t;
		return;
	}

	if (dtb->type == DT_ULONG) {
		t = alloc_reg (copy_dt (&dt_ulong), 0);
		printf ("\tlet $%d: dword = zext $%d;\n", t, *a);
		*a = t;
		return;
	}

	if (dta->type == DT_LONG && dtb->type == DT_UINT) {
		t = alloc_reg (copy_dt (&dt_ulong), 0);
		u = alloc_reg (copy_dt (&dt_ulong), 0);
		printf ("\tlet $%d: dword = $%d;\n", t, *a);
		printf ("\tlet $%d: dword = zext $%d;\n", u, *b);
		*a = t;
		*b = u;
		return;
	}

	if (dta->type == DT_UINT && dtb->type == DT_LONG) {
		t = alloc_reg (copy_dt (&dt_ulong), 0);
		u = alloc_reg (copy_dt (&dt_ulong), 0);
		printf ("\tlet $%d: dword = zext $%d;\n", t, *a);
		printf ("\tlet $%d: dword = $%d;\n", u, *b);
		*a = t;
		*b = u;
		return;
	}

	if (dta->type == DT_LONG) {
		assert (dtb->type == DT_INT);
		t = alloc_reg (copy_dt (&dt_long), 0);
		printf ("\tlet $%d: dword = sext $%d;\n", t, *b);
		*b = t;
		return;
	}

	if (dtb->type == DT_LONG) {
		assert (dta->type == DT_INT);
		t = alloc_reg (copy_dt (&dt_long), 0);
		printf ("\tlet $%d: dword = sext $%d;\n", t, *a);
		*a = t;
		return;
	}

	if (dta->type == DT_UINT) {
		assert (dtb->type == DT_INT);
		t = alloc_reg (copy_dt (&dt_uint), 0);
		printf ("\tlet $%d: word = $%d;\n", t, *b);
		*b = t;
		return;
	}

	if (dtb->type == DT_UINT) {
		assert (dta->type == DT_INT);
		t = alloc_reg (copy_dt (&dt_uint), 0);
		printf ("\tlet $%d: word = $%d;\n", t, *a);
		*a = t;
		return;
	}

	/* should be unreachable */
	errx (1, "dta=%d, dtb=%d", dta->type, dtb->type);
}

int
do_ptr_add (ptr, off)
{
	struct dtype *dtp, *dto;
	int r, sz, off2;
	char *instr;

	ptr = rvalue (ptr);
	off = rvalue (promote (off));
	dtp = regs[ptr].r_dt;
	dto = regs[off].r_dt;

	assert (dtp->type == DT_PTR);

	switch (dto->type) {
	case DT_INT:
	case DT_UINT:
		break;
	case DT_LONG:
	case DT_ULONG:
		errx (1, "TODO: add ptr + (u)long");
	default:
		errx (1, "cannot add ptr and %d", dto->type);
	}

	sz = sizeof_dt (dtp->inner);
	off2 = alloc_reg (copy_dt (dto), 0);
	printf ("\tlet $%d: ", off2);
	print_dt (dto);
	instr = is_signed (dto) ? "smul" : "umul";
	printf (" = %s $%d, %d;\n", instr, off, sz);

	r = alloc_reg (copy_dt (dtp), 0);
	printf ("\tlet $%d: ptr = add $%d, $%d;\n", r, ptr, off2);

	return r;
}

int
do_add (a, b)
{
	struct dtype *dt;
	int r;

	if (regs[a].r_dt->type == DT_PTR) {
		r = do_ptr_add (a, b);
	} else if (regs[b].r_dt->type == DT_PTR) {
		r = do_ptr_add (b, a);
	} else {
		promote2 (&a, &b);
		dt = regs[a].r_dt;
		r = alloc_reg (copy_dt (dt), 0);

		switch (dt->type) {
		case DT_INT:
		case DT_UINT:
		case DT_LONG:
		case DT_ULONG:
			printf ("\tlet $%d: ", r);
			print_dt (dt);
			printf (" = add $%d, $%d;\n", a, b);
			break;
		default:
			abort ();
		}
	}

	return r;
}

int
do_sub (a, b)
{
	struct dtype *dt, *dta, *dtb;
	char *instr;
	int r, t, sz;

	dta = regs[a].r_dt;
	dtb = regs[b].r_dt;

	if (dta->type == DT_PTR && dtb->type == DT_PTR) {
		assert_dt_eq (dta, dtb);
		sz = sizeof_dt (dta->inner);

		t = alloc_reg (copy_dt (&dt_int), 0);
		printf ("\tlet $%d: word = sub $%d, $%d;\n", t, a, b);
		r = alloc_reg (copy_dt (&dt_int), 0);
		printf ("\tlet $%d: word = udiv $%d, %d;\n", r, t, sz);
	} else if (dta->type == DT_PTR) {
		sz = sizeof_dt (dta->inner);
		switch (dtb->type) {
		case DT_INT:
		case DT_UINT:
			instr = is_signed (dtb) ? "smul" : "umul";
			t = alloc_reg (copy_dt (&dt_int), 0);
			printf ("\tlet $%d: word = %s $%d, %d;\n", t, instr, b, sz);
			r = alloc_reg (copy_dt (dta), 0);
			printf ("\tlet $%d: ptr = sub $%d, $%d;\n", r, a, t);
			break;
		default:
			errx (1, "cannot subtract %d from ptr", dtb->type);
		}
	} else {
		promote2 (&a, &b);
		dt = regs[a].r_dt;
		r = alloc_reg (copy_dt (dt), 0);

		switch (dt->type) {
		case DT_INT:
		case DT_UINT:
		case DT_LONG:
		case DT_ULONG:
			printf ("\tlet $%d: ", r);
			print_dt (dt);
			printf (" = sub $%d, $%d;\n", a, b);
			break;
		default:
			abort ();
		}
	}

	return r;
}

int
add ()
{
	int r, a, b;
	char op;

	r = unary ();

	while (1) {
		switch (peek ()) {
		case TOK_PLUS:
			op = '+';
			break;
		case TOK_MINUS:
			op = '-';
			break;
		default:
			goto end;
		}

		next ();

		a = rvalue (r);
		b = rvalue (unary ());
		a = promote (a);
		b = promote (b);
		r = op == '+' ? do_add (a, b) : do_sub (a, b);
	}

end:
	return r;
}

int
tern ()
{
	int r, a, b, lt, lf, le;

	r = add ();

	if (!match (TOK_QMARK))
		return r;

	r = rvalue (r);

	lt = alloc_reg (copy_dt (&dt_lbl), 1);
	lf = alloc_reg (copy_dt (&dt_lbl), 1);
	le = alloc_reg (copy_dt (&dt_lbl), 1);

	printf ("\tbr $%d, $%d, $%d;\n\n", r, lt, lf);

	printf ("%d:\n", lt);
	a = promote (rvalue (expr ()));
	printf ("\tjmp $%d;\n\n", le);

	expect (TOK_COLON);

	printf ("%d:\n", lf);
	b = promote (rvalue (expr ()));
	printf ("\tjmp $%d;\n\n", le);
	
	printf ("%d:\n", le);
	// TODO: check for common type
	r = alloc_reg (copy_dt (regs[a].r_dt), 0);
	printf ("\tlet $%d: ", r);
	print_dt (regs[r].r_dt);
	printf (" = phi [$%d, $%d], [$%d, $%d];\n", lt, a, lf, b);
	return r;
}

int
comma ()
{
	int r;
	do {
		r = tern ();
	} while (match (TOK_COMMA));
	return r;
}

int
expr ()
{
	return comma ();
}

#define F_SIGNED 0x01
#define F_UNSIGNED 0x02

enum base_type {
	BT_UNKNOWN,
	BT_CHAR,
	BT_SHORT,
	BT_INT,
	BT_LONG,
};

struct dtype *
dtype ()
{
	enum base_type base = BT_UNKNOWN;
	int flags = 0;

	for (flags = 0; ; next ()) {
		switch (peek ()) {
		case KW_CHAR:
			assert (base == BT_UNKNOWN);
			base = BT_CHAR;
			break;
		case KW_SHORT:
			assert (base == BT_UNKNOWN || base == BT_INT);
			base = BT_SHORT;
			break;
		case KW_INT:
			if (base == BT_UNKNOWN) {
				base = BT_INT;
			} else {
				assert (base != BT_CHAR);
			}
			break;
		case KW_LONG:
			assert (base == BT_UNKNOWN || base == BT_INT);
			base = BT_LONG;
			break;
		case KW_SIGNED:
			flags |= F_SIGNED;
			break;
		case KW_UNSIGNED:
			flags |= F_UNSIGNED;
			break;
		default:
			goto done;
		}
	}

done:
	if ((flags & (F_SIGNED | F_UNSIGNED)) == (F_SIGNED | F_UNSIGNED))
		errx (1, "cannot combine signed and unsigned");

	switch (base) {
	case BT_CHAR:
		if (flags & F_SIGNED) {
			return copy_dt (&dt_schar);
		} else if (flags & F_UNSIGNED) {
			return copy_dt (&dt_uchar);
		} else {
			return copy_dt (&dt_char);
		}
	case BT_SHORT:
		return copy_dt (flags & F_UNSIGNED ? &dt_ushort : &dt_short);
	case BT_UNKNOWN:
	case BT_INT:
		return copy_dt (flags & F_UNSIGNED ? &dt_uint : &dt_int);
	case BT_LONG:
		return copy_dt (flags & F_UNSIGNED ? &dt_ulong : &dt_long);
	default:
		abort ();
	}
}

void
stmt ()
{
	int r;
	switch (peek ()) {
	case KW_RETURN:
		next ();
		if (peek () == TOK_SEMI) {
			printf ("\tret;\n");
		} else {
			r = rvalue (expr ());
			printf ("\tret $%d;\n", r);
		}
		expect (TOK_SEMI);
		break;
	default:
		r = expr ();
		break;
	}
}

enum level {
	L_GLOBAL_TOP,
	L_GLOBAL,
	// L_ARGS,
	L_LOCAL,
};

enum level
lvl_decay (lvl)
enum level lvl;
{
	switch (lvl) {
	case L_GLOBAL_TOP:
		return L_GLOBAL;
	default:
		return lvl;
	}
}

void
decl2 (dt, sym, lvl)
struct dtype **dt;
struct symbol *sym;
enum level lvl;
{
	extern int decl1 ();

	switch (next ()) {
	case TOK_IDENT:
		sym->dt = ptr_dt (*dt, 0);
		copyident (sym->id, lval.id);
		break;
	case TOK_STAR:
		*dt = ptr_dt (*dt, 0);
		decl2 (dt, sym, lvl);
		break;
	case TOK_LPAR:
		decl1 (dt, sym, lvl_decay (lvl));
		expect (TOK_RPAR);
		break;
	default:
		errx (1, "expected declaration");
	}
}

int
is_func_begin ()
{
	return peek () == TOK_LCURLY;
}

void
func (sym)
struct symbol *sym;
{
	int decl ();

	gvars = sym;

	printf ("fn %s() {\n", sym->id);

	expect (TOK_LCURLY);
	while (decl (L_LOCAL));

	while (peek () != TOK_RCURLY)
		stmt ();

	expect (TOK_RCURLY);
	printf ("}\n\n");
	reset_regs ();

}

int
decl1 (dt, sym, lvl)
struct dtype **dt;
struct symbol *sym;
enum level lvl;
{
	struct dtype *ndt;

	decl2 (dt, sym, lvl);

	if (match (TOK_LPAR)) {
		// TODO: arguments
		expect (TOK_RPAR);
		ndt = new (struct dtype);
		ndt->type = DT_FUNC;
		ndt->inner = sym->dt;
		ndt->refcnt = 1;
		sym->dt = ndt;

		if (lvl == L_GLOBAL_TOP && is_func_begin ()) {
			func (sym);
			return 1;
		}

	}

	// TODO: arrays

	return 0;
}

struct dtype *
try_dtype ()
{
	switch (peek ()) {
	case KW_SIGNED:
	case KW_UNSIGNED:
	case KW_CHAR:
	case KW_SHORT:
	case KW_INT:
	case KW_LONG:
		return dtype ();
	default:
		return NULL;
	}
}

int
decl (lvl)
enum level lvl;
{
	struct symbol *sym;
	struct dtype *dt, *dt2;

	dt = try_dtype ();
	if (dt == NULL) {
		if (lvl == L_GLOBAL || lvl == L_GLOBAL_TOP) {
			dt = copy_dt (&dt_int);
		} else {
			return 0;
		}
	}
	
	do {
		sym = new (struct symbol);
		switch (lvl) {
		case L_GLOBAL_TOP:
		case L_GLOBAL:
			sym->next = gvars;
			break;
		case L_LOCAL:
			sym->next = fvars;
			break;
		}
		dt2 = copy_dt (dt);

		if (decl1 (&dt2, sym, lvl)) {
			free_dt (dt);
			return 1;
		} else {
			switch (lvl) {
			case L_GLOBAL_TOP:
			case L_GLOBAL:
				gvars = sym;
				switch (sym->dt->type) {
				case DT_PTR:
					printf ("static %s: ", sym->id);
					print_dt (sym->dt->inner);
					printf (";\n\n");
					break;
				case DT_FUNC:
					printf ("# extern %s;\n", sym->id);
					break;
				default:
					abort ();
				}
				sym->reg = -1;
				break;
			case L_LOCAL:
				fvars = sym;
				switch (sym->dt->type) {
				case DT_PTR:
					sym->reg = alloc_reg (sym->dt, 1);
					printf ("\tlet $%d: ptr = alloc %d;\t# %s\n",
							sym->reg, sizeof_dt (sym->dt->inner), sym->id);
					break;
				case DT_FUNC:
					sym->reg = -1;
					printf ("\t# extern %s;\n", sym->id);
					break;
				default:
					abort ();
				}
				break;
			}
		}

	} while (match (TOK_COMMA));

	free_dt (dt);
	expect (TOK_SEMI);
	return 1;
}


int main ()
{
	cm = CM_SMALL;
	memset (regs, 0, sizeof (regs));
	while (peek () != TOK_EOF) {
		decl (L_GLOBAL_TOP);
	}
}
