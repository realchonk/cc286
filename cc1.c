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

#define unreachable(msg) errx (1, "line %d: %s: unreachable: %s", __LINE__, __func__, msg)

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
	TOK_PERC,
	TOK_LPAR,
	TOK_RPAR,
	TOK_LCURLY,
	TOK_RCURLY,
	TOK_COMMA,
	TOK_QMARK,
	TOK_COLON,
	TOK_PERIOD,
	TOK_ARROW,
	TOK_EQ,

	KW_RETURN,
	KW_SIGNED,
	KW_UNSIGNED,
	KW_CHAR,
	KW_SHORT,
	KW_INT,
	KW_LONG,
	KW_GOTO,
	KW_ENUM,
	KW_STRUCT,
	KW_UNION,

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
		} else if (strcmp (lval.id, "goto") == 0) {
			return KW_GOTO;
		} else if (strcmp (lval.id, "enum") == 0) {
			return KW_ENUM;
		} else if (strcmp (lval.id, "struct") == 0) {
			return KW_STRUCT;
		} else if (strcmp (lval.id, "union") == 0) {
			return KW_UNION;
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
		ch = getchar ();
		if (ch == '>') {
			return TOK_ARROW;
		} else {
			ungetc (ch, stdin);
			return TOK_MINUS;
		}
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
	case '%':
		return TOK_PERC;
	case '?':
		return TOK_QMARK;
	case ':':
		return TOK_COLON;
	case '.':
		return TOK_PERIOD;
	case '=':
		return TOK_EQ;
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

int
matches (exp)
enum token_type exp;
{
	return peek () == exp;
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
	DT_STRUCT,
	DT_UNION,
};

struct structure {
	char		 su_id[MAXIDENT + 1];
	struct symbol	*su_members;
	size_t		 su_size;
};

struct dtype {
	enum dtype_type		 dt_type;
	int			 dt_refcnt;
	union {
		struct dtype	*dtv_inner;
		struct structure*dtv_su;
	} dt_v;
};

#define dt_inner dt_v.dtv_inner
#define dt_su dt_v.dtv_su

#define builtin(ty) { ty, 1, { NULL } }
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
	dt->dt_refcnt++;
	return dt;
}

void
print_dt (dt)
struct dtype *dt;
{
	switch (dt->dt_type) {
	case DT_NULL:
		unreachable ("DT_NULL");
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
	case DT_STRUCT:
	case DT_UNION:
		unreachable ("struct/union");
	}
}

struct dtype *
new_dt (type)
enum dtype_type type;
{
	struct dtype *dt;
	dt = new (struct dtype);
	dt->dt_type = type;
	dt->dt_refcnt = 1;
	return dt;
}

void
free_dt (dt)
struct dtype *dt;
{
	--dt->dt_refcnt;
	if (dt->dt_refcnt != 0)
		return;

	switch (dt->dt_type) {
	case DT_NULL:
		unreachable ("DT_NULL");
	case DT_PTR:
	case DT_FUNC:
		free_dt (dt->dt_inner);
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
	case DT_STRUCT:
	case DT_UNION:
		/* TODO: free members */
		free (dt->dt_su);
		break;
	}
	free (dt);
}

int
sizeof_dt (dt)
struct dtype *dt;
{
	switch (dt->dt_type) {
	case DT_NULL:
		unreachable ("DT_NULL");
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
		/* TODO: code models */
		unreachable ("DT_LBL");
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
	case DT_STRUCT:
	case DT_UNION:
		return dt->dt_su->su_size;
	}
}

struct dtype *
ptr_dt (inner, inc)
struct dtype *inner;
{
	struct dtype *ptr;

	ptr = new_dt (DT_PTR);
	ptr->dt_inner = inner;

	if (inc)
		++inner->dt_refcnt;

	return ptr;
}

void
assert_dt_eq (dta, dtb)
struct dtype *dta, *dtb;
{
	if (dta->dt_type != dtb->dt_type)
		errx (1, "assert_dt_eq(): %d != %d", dta->dt_type, dtb->dt_type);

	switch (dta->dt_type) {
	case DT_PTR:
	case DT_FUNC:
		return assert_dt_eq (dta->dt_inner, dtb->dt_inner);
	default:
		break;
	}
}

int
is_signed (dt)
struct dtype *dt;
{
	switch (dt->dt_type) {
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
		unreachable ("not an integer");
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

enum namespace {
	NS_VAR,
	NS_ENUM,
	NS_STRUCT,
	NS_UNION,
};

#define SYM_INVALID (-1)
#define SYM_NAMED (-2)
#define SYM_CONST (-3)
struct symbol {
	struct symbol	*sym_next;
	enum namespace	 sym_ns;
	char		 sym_id[MAXIDENT + 1];
	struct dtype	*sym_dt;
	int		 sym_reg;
	int		 sym_val;
};

static struct symbol *fvars = NULL, *gvars = NULL;

struct symbol *
new_sym (ns, id, dt, reg)
enum namespace	 ns;
char		*id;
struct dtype	*dt;
{
	struct symbol *sym;
	sym = new (struct symbol);
	sym->sym_next	= NULL;
	sym->sym_ns	= ns;
	sym->sym_dt	= dt;
	sym->sym_reg	= reg;
	sym->sym_val	= -1;
	copyident (sym->sym_id, id);
	return sym;
}

void
put_sym (head, sym)
struct symbol **head, *sym;
{
	assert (sym->sym_next == NULL);
	assert (sym->sym_id[0] != '\0');
	sym->sym_next = *head;
	*head = sym;
}

struct symbol *
lookup_in (head, id, ns)
struct symbol *head;
char *id;
enum namespace ns;
{
	for (; head != NULL; head = head->sym_next) {
		if (ns == head->sym_ns && strcmp (head->sym_id, id) == 0)
			return head;
	}
	return NULL;
}

struct symbol *
lookup (id, ns)
char *id;
enum namespace ns;
{
	struct symbol *sym;

	if ((sym = lookup_in (fvars, id, ns)) != NULL)
		return sym;

	if ((sym = lookup_in (gvars, id, ns)) != NULL)
		return sym;

	return NULL;
}

/* PARSER */

int
rvalue (r)
{
	struct reg *reg;
	int x;

	reg = &regs[r];
	if (!reg->r_is_lv || reg->r_dt->dt_type == DT_FUNC) 
		return r;

	assert (reg->r_dt->dt_type == DT_PTR);
	x = alloc_reg (copy_dt (reg->r_dt->dt_inner), 0);
	printf ("\tlet $%d: ", x);
	print_dt (reg->r_dt->dt_inner);
	printf (" = read $%d;\n", r);
	return x;
}

int
lvalue (r)
{
	if (!regs[r].r_is_lv)
		errx (1, "must be an lvalue");
	assert (regs[r].r_dt->dt_type == DT_PTR || regs[r].r_dt->dt_type == DT_FUNC);
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
		sym = lookup (lval.id, NS_VAR);
		if (sym == NULL) {
			if (match (TOK_COLON)) {
				printf ("\tjmp %s;\n\n", lval.id);
				printf ("%s:\n", lval.id);
				return -1;
			}
			errx (1, "invalid symbol: %s", lval.id);
		}
		switch (sym->sym_reg) {
		case SYM_INVALID:
			errx (1, "cannot access %s", lval.id);
		case SYM_NAMED:
			r = alloc_reg (copy_dt (sym->sym_dt), 1);
			printf ("\tlet $%d: ", r);
			print_dt (sym->sym_dt);
			printf (" = load %s;\n", sym->sym_id);
			break;
		case SYM_CONST:
			r = alloc_reg (copy_dt (sym->sym_dt), 0);
			printf ("\tlet $%d: ", r);
			print_dt (sym->sym_dt);
			printf (" = %d;\n", sym->sym_val);
			break;
		default:
			r = sym->sym_reg;
			break;
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

int member ()
{
	struct symbol	*sym;
	struct dtype	*dt;
	int		 r, s;

	r = atom ();

	while (1) {
		switch (peek ()) {
		case TOK_PERIOD:
			s = lvalue (r);
			goto common;

		case TOK_ARROW:
			s = rvalue (r);
			goto common;

		common:
			next ();
			dt = regs[s].r_dt->dt_inner;
			assert (dt->dt_type == DT_STRUCT || dt->dt_type == DT_UNION);

			expect (TOK_IDENT);
			sym = lookup_in (dt->dt_su->su_members, lval.id, NS_VAR);
			if (sym == NULL)
				errx (1, "no member '%s' for struct/union '%s'", lval.id, dt->dt_su->su_id);

			r = alloc_reg (copy_dt (sym->sym_dt), 1);
			printf ("\tlet $%d: ptr = add $%d, %d\n", r, s, sym->sym_reg);
			break;
		default:
			return r;
		}
	}
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
		r = member ();
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

	switch (adt->dt_type) {
	case DT_NULL:
	case DT_LBL:
		unreachable ("DT_NULL or DT_LBL");
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
	case DT_STRUCT:
	case DT_UNION:
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

	if (dta->dt_type == dtb->dt_type)
		return;

	if (dta->dt_type == DT_ULONG) {
		t = alloc_reg (copy_dt (&dt_ulong), 0);
		printf ("\tlet $%d: dword = zext $%d;\n", t, *b);
		*b = t;
		return;
	}

	if (dtb->dt_type == DT_ULONG) {
		t = alloc_reg (copy_dt (&dt_ulong), 0);
		printf ("\tlet $%d: dword = zext $%d;\n", t, *a);
		*a = t;
		return;
	}

	if (dta->dt_type == DT_LONG && dtb->dt_type == DT_UINT) {
		t = alloc_reg (copy_dt (&dt_ulong), 0);
		u = alloc_reg (copy_dt (&dt_ulong), 0);
		printf ("\tlet $%d: dword = $%d;\n", t, *a);
		printf ("\tlet $%d: dword = zext $%d;\n", u, *b);
		*a = t;
		*b = u;
		return;
	}

	if (dta->dt_type == DT_UINT && dtb->dt_type == DT_LONG) {
		t = alloc_reg (copy_dt (&dt_ulong), 0);
		u = alloc_reg (copy_dt (&dt_ulong), 0);
		printf ("\tlet $%d: dword = zext $%d;\n", t, *a);
		printf ("\tlet $%d: dword = $%d;\n", u, *b);
		*a = t;
		*b = u;
		return;
	}

	if (dta->dt_type == DT_LONG) {
		assert (dtb->dt_type == DT_INT);
		t = alloc_reg (copy_dt (&dt_long), 0);
		printf ("\tlet $%d: dword = sext $%d;\n", t, *b);
		*b = t;
		return;
	}

	if (dtb->dt_type == DT_LONG) {
		assert (dta->dt_type == DT_INT);
		t = alloc_reg (copy_dt (&dt_long), 0);
		printf ("\tlet $%d: dword = sext $%d;\n", t, *a);
		*a = t;
		return;
	}

	if (dta->dt_type == DT_UINT) {
		assert (dtb->dt_type == DT_INT);
		t = alloc_reg (copy_dt (&dt_uint), 0);
		printf ("\tlet $%d: word = $%d;\n", t, *b);
		*b = t;
		return;
	}

	if (dtb->dt_type == DT_UINT) {
		assert (dta->dt_type == DT_INT);
		t = alloc_reg (copy_dt (&dt_uint), 0);
		printf ("\tlet $%d: word = $%d;\n", t, *a);
		*a = t;
		return;
	}

	/* should be unreachable */
	errx (1, "dta=%d, dtb=%d", dta->dt_type, dtb->dt_type);
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

	assert (dtp->dt_type == DT_PTR);

	switch (dto->dt_type) {
	case DT_INT:
	case DT_UINT:
		break;
	case DT_LONG:
	case DT_ULONG:
		errx (1, "TODO: add ptr + (u)long");
	default:
		errx (1, "cannot add ptr and %d", dto->dt_type);
	}

	sz = sizeof_dt (dtp->dt_inner);
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

	if (regs[a].r_dt->dt_type == DT_PTR) {
		r = do_ptr_add (a, b);
	} else if (regs[b].r_dt->dt_type == DT_PTR) {
		r = do_ptr_add (b, a);
	} else {
		promote2 (&a, &b);
		dt = regs[a].r_dt;
		r = alloc_reg (copy_dt (dt), 0);

		switch (dt->dt_type) {
		case DT_INT:
		case DT_UINT:
		case DT_LONG:
		case DT_ULONG:
			printf ("\tlet $%d: ", r);
			print_dt (dt);
			printf (" = add $%d, $%d;\n", a, b);
			break;
		default:
			unreachable ("unexpected dtype");
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

	if (dta->dt_type == DT_PTR && dtb->dt_type == DT_PTR) {
		assert_dt_eq (dta, dtb);
		sz = sizeof_dt (dta->dt_inner);

		t = alloc_reg (copy_dt (&dt_int), 0);
		printf ("\tlet $%d: word = sub $%d, $%d;\n", t, a, b);
		r = alloc_reg (copy_dt (&dt_int), 0);
		printf ("\tlet $%d: word = udiv $%d, %d;\n", r, t, sz);
	} else if (dta->dt_type == DT_PTR) {
		sz = sizeof_dt (dta->dt_inner);
		switch (dtb->dt_type) {
		case DT_INT:
		case DT_UINT:
			instr = is_signed (dtb) ? "smul" : "umul";
			t = alloc_reg (copy_dt (&dt_int), 0);
			printf ("\tlet $%d: word = %s $%d, %d;\n", t, instr, b, sz);
			r = alloc_reg (copy_dt (dta), 0);
			printf ("\tlet $%d: ptr = sub $%d, $%d;\n", r, a, t);
			break;
		default:
			errx (1, "cannot subtract %d from ptr", dtb->dt_type);
		}
	} else {
		promote2 (&a, &b);
		dt = regs[a].r_dt;
		r = alloc_reg (copy_dt (dt), 0);

		switch (dt->dt_type) {
		case DT_INT:
		case DT_UINT:
		case DT_LONG:
		case DT_ULONG:
			printf ("\tlet $%d: ", r);
			print_dt (dt);
			printf (" = sub $%d, $%d;\n", a, b);
			break;
		default:
			unreachable ("unexpected dtype");
		}
	}

	return r;
}

int
do_mdm (instr, a, b)
char *instr;
{
	struct dtype	*dta, *dtb;
	char		 prefix;
	int		 r;

	promote2 (&a, &b);
	dta = regs[a].r_dt;
	dtb = regs[b].r_dt;
	assert_dt_eq (dta, dtb);

	prefix = is_signed (dta) ? 's' : 'u';

	r = alloc_reg (copy_dt (dta), 0);

	printf ("\tlet $%d: ", r);
	print_dt (dta);
	printf (" = %c%s $%d, $%d;\n", prefix, instr, a, b);

	return r;
}

int
mul ()
{
	int r, a, b;
	char *instr;

	r = unary ();

	while (1) {
		switch (peek ()) {
		case TOK_STAR:
			instr = "mul";
			break;
		case TOK_SLASH:
			instr = "div";
			break;
		case TOK_PERC:
			instr = "mod";
			break;
		default:
			return r;
		}

		next ();

		a = rvalue (r);
		b = rvalue (unary ());
		a = promote (a);
		b = promote (b);
		r = do_mdm (instr, a, b);
	}
}

int
add ()
{
	int r, a, b;
	char op;

	r = mul ();

	while (1) {
		switch (peek ()) {
		case TOK_PLUS:
			op = '+';
			break;
		case TOK_MINUS:
			op = '-';
			break;
		default:
			return r;
		}

		next ();

		a = rvalue (r);
		b = rvalue (mul ());
		a = promote (a);
		b = promote (b);
		r = op == '+' ? do_add (a, b) : do_sub (a, b);
	}
}

int
tern ()
{
	int r, a, b, lt, lf, le, assign ();

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
	b = promote (rvalue (assign ()));
	printf ("\tjmp $%d;\n\n", le);
	
	printf ("%d:\n", le);
	/* TODO: check for common type */
	r = alloc_reg (copy_dt (regs[a].r_dt), 0);
	printf ("\tlet $%d: ", r);
	print_dt (regs[r].r_dt);
	printf (" = phi [$%d, $%d], [$%d, $%d];\n", lt, a, lf, b);
	return r;
}

int
assign ()
{
	int l, r;

	l = tern ();

	if (!match (TOK_EQ))
		return l;

	l = lvalue (l);
	r = rvalue (assign ());
	/* TODO: implicit cast */

	assert_dt_eq (regs[l].r_dt->dt_inner, regs[r].r_dt);

	printf ("\twrite $%d, $%d;\n", l, r);

	return l;
}

int
comma ()
{
	int r;
	do {
		r = assign ();
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
	BT_ENUM,
	BT_STRUCT,
	BT_UNION,
};

enum level {
	LVL_GLOBAL_TOP,
	LVL_GLOBAL,
	/* LVL_ARGS, */
	LVL_LOCAL,
	LVL_STRUCT,
	LVL_UNION,
};

struct dtype *
dtype (scope)
struct symbol **scope;
{
	enum base_type	 base = BT_UNKNOWN;
	enum level	 lvl;
	enum dtype_type	 dtt;
	enum namespace	 ns;
	struct symbol	*sym = NULL, *sym2;
	struct dtype	*dt;
	char		 id[MAXIDENT + 1];
	int		 i, flags = 0, decl ();

	for (flags = 0; ;) {
		switch (peek ()) {
		case KW_CHAR:
			assert (base == BT_UNKNOWN);
			base = BT_CHAR;
			next ();
			break;
		case KW_SHORT:
			assert (base == BT_UNKNOWN || base == BT_INT);
			base = BT_SHORT;
			next ();
			break;
		case KW_INT:
			if (base == BT_UNKNOWN) {
				base = BT_INT;
			} else {
				assert (base != BT_CHAR);
			}
			next ();
			break;
		case KW_LONG:
			assert (base == BT_UNKNOWN || base == BT_INT);
			base = BT_LONG;
			next ();
			break;
		case KW_SIGNED:
			flags |= F_SIGNED;
			next ();
			break;
		case KW_UNSIGNED:
			flags |= F_UNSIGNED;
			next ();
			break;
		case KW_ENUM:
			next ();
			if (base != BT_UNKNOWN)
				errx (1, "type error");
			base = BT_ENUM;

			if (match (TOK_IDENT)) {
				copyident (id, lval.id);
			} else {
				id[0] = '\0';
			}

			if (!match (TOK_LCURLY)) {
				sym = lookup (id, NS_ENUM);
				break;
			}

			if (*id != '\0') {
				sym = new_sym (NS_ENUM, id, copy_dt (&dt_int), SYM_INVALID);
				put_sym (scope, sym);
			}

			i = 0;

			do {
				if (matches (TOK_RCURLY))
					break;

				/* TODO: NAME = value */
				expect (TOK_IDENT);
				sym2 = new_sym (NS_VAR, lval.id, copy_dt (&dt_int), SYM_CONST);
				sym2->sym_val = i++;
				put_sym (scope, sym2);
			} while (match (TOK_COMMA));

			expect (TOK_RCURLY);
			break;
		case KW_STRUCT:
			lvl = LVL_STRUCT;
			dtt = DT_STRUCT;
			ns = NS_STRUCT;
			goto common_su;
		case KW_UNION:
			lvl = LVL_UNION;
			dtt = DT_UNION;
			ns = NS_UNION;
			goto common_su;
	
		common_su:
			next ();
			if (base != BT_UNKNOWN)
				errx (1, "type error");

			base = BT_STRUCT;

			if (match (TOK_IDENT)) {
				copyident (id, lval.id);
			} else {
				id[0] = '\0';
			}

			if (!match (TOK_LCURLY)) {
				sym = lookup (id, ns);
				if (sym == NULL)
					errx (1, "invalid struct/union: '%s'", id);
				dt = sym->sym_dt;
				break;
			}

			dt = new_dt (dtt);
			dt->dt_su = new (struct structure);
			dt->dt_su->su_members = NULL;
			dt->dt_su->su_size = 0;
			copyident (dt->dt_su->su_id, id);

			while (!match (TOK_RCURLY)) {
				decl (lvl, &dt->dt_su->su_members, &dt->dt_su->su_size);
			}

			if (*id != '\0') {
				sym = new_sym (NS_STRUCT, id, copy_dt (dt), SYM_INVALID);
				put_sym (scope, sym);
			}
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
	case BT_ENUM:
		return copy_dt (&dt_int);
	case BT_STRUCT:
	case BT_UNION:
		return dt;
	}
}

struct dtype *
try_dtype (scope)
struct symbol **scope;
{
	switch (peek ()) {
	case KW_SIGNED:
	case KW_UNSIGNED:
	case KW_CHAR:
	case KW_SHORT:
	case KW_INT:
	case KW_LONG:
	case KW_ENUM:
	case KW_STRUCT:
	case KW_UNION:
		return dtype (scope);
	default:
		return NULL;
	}
}

void
stmt ()
{
	int r;
	switch (peek ()) {
	case KW_RETURN:
		next ();
		if (matches (TOK_SEMI)) {
			printf ("\tret;\n\n");
		} else {
			r = rvalue (expr ());
			printf ("\tret $%d;\n\n", r);
		}
		expect (TOK_SEMI);
		break;
	case KW_GOTO:
		next ();
		expect (TOK_IDENT);
		printf ("\tjmp %s;\n\n", lval.id);
		expect (TOK_SEMI);
		break;
	case TOK_SEMI:
		next ();
		break;
	default:
		r = expr ();
		if (r != -1)
			expect (TOK_SEMI);
		break;
	}
}

enum level
lvl_decay (lvl)
enum level lvl;
{
	switch (lvl) {
	case LVL_GLOBAL_TOP:
		return LVL_GLOBAL;
	default:
		return lvl;
	}
}

int
is_func_begin ()
{
	return matches (TOK_LCURLY);
}

void
func (sym)
struct symbol *sym;
{
	int decl ();

	/* TODO: free fvars */
	fvars = NULL;

	sym->sym_reg = SYM_NAMED;
	put_sym (&gvars, sym);

	printf ("fn %s() {\n", sym->sym_id);

	expect (TOK_LCURLY);

	while (decl (LVL_LOCAL, &fvars, NULL));

	while (!matches (TOK_RCURLY))
		stmt ();

	expect (TOK_RCURLY);
	printf ("}\n\n");
	reset_regs ();
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
		sym->sym_dt = ptr_dt (*dt, 0);
		copyident (sym->sym_id, lval.id);
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
decl1 (dt, sym, lvl)
struct dtype **dt;
struct symbol *sym;
enum level lvl;
{
	struct dtype *ndt;

	decl2 (dt, sym, lvl);

	if (match (TOK_LPAR)) {
		/* TODO: arguments */
		expect (TOK_RPAR);
		ndt = new_dt (DT_FUNC);
		ndt->dt_inner = sym->sym_dt;
		sym->sym_dt = ndt;

		if (lvl == LVL_GLOBAL_TOP && is_func_begin ()) {
			func (sym);
			return 1;
		}
	}

	/* TODO: arrays */

	return 0;
}

int
decl (lvl, scope, arg)
enum level	  lvl;
struct symbol	**scope;
void		 *arg;
{
	struct symbol	*sym;
	struct dtype	*dt, *dt2;
	size_t		*addr, sz;

	dt = try_dtype (scope);
	if (dt == NULL) {
		if (lvl == LVL_GLOBAL || lvl == LVL_GLOBAL_TOP) {
			dt = copy_dt (&dt_int);
		} else {
			return 0;
		}
	}

	if (matches (TOK_SEMI))
		goto end;

	do {
		sym = new (struct symbol);
		dt2 = copy_dt (dt);

		if (decl1 (&dt2, sym, lvl)) {
			free_dt (dt);
			return 1;
		}

		switch (lvl) {
		case LVL_GLOBAL_TOP:
		case LVL_GLOBAL:
			switch (sym->sym_dt->dt_type) {
			case DT_PTR:
				printf ("static %s: ", sym->sym_id);
				print_dt (sym->sym_dt->dt_inner);
				printf (";\n\n");
				break;
			case DT_FUNC:
				printf ("# extern %s;\n", sym->sym_id);
				break;
			default:
				abort ();
			}
			sym->sym_reg = SYM_NAMED;
			break;
		case LVL_LOCAL:
			switch (sym->sym_dt->dt_type) {
			case DT_PTR:
				sym->sym_reg = alloc_reg (sym->sym_dt, 1);
				printf ("\tlet $%d: ptr = alloc %d;\t# %s\n",
					sym->sym_reg, sizeof_dt (sym->sym_dt->dt_inner), sym->sym_id);
				break;
			case DT_FUNC:
				sym->sym_reg = SYM_NAMED;
				printf ("\t# extern %s;\n", sym->sym_id);
				break;
			default:
				abort ();
			}
			break;
		case LVL_STRUCT:
			addr = (size_t *)arg;
			sym->sym_reg = *addr;
			*addr += sizeof_dt (sym->sym_dt);
			break;
		case LVL_UNION:
			addr = (size_t *)arg;
			sym->sym_reg = 0;
			sz = sizeof_dt (sym->sym_dt);
			if (sz > *addr)
				*addr = sz;
			break;
		}

		put_sym (scope, sym);
	} while (match (TOK_COMMA));

end:
	free_dt (dt);
	expect (TOK_SEMI);
	return 1;
}


int main ()
{
	cm = CM_SMALL;
	memset (regs, 0, sizeof (regs));
	while (!matches (TOK_EOF))
		decl (LVL_GLOBAL_TOP, &gvars, NULL);
	return 0;
}
