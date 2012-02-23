#include <sys/types.h>
#include <errno.h>
#include <grp.h>
#include <string.h>
#include <unistd.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>

static void
error(const char *s)
{
    char *exn = "Release_privileges.Release_privileges_error";
    caml_raise_with_string(*caml_named_value(exn), s);
}

CAMLprim value
ocaml_setresuid(value r, value e, value s)
{
    CAMLparam3(r, e, s);
    uid_t ruid = Int_val(r);
    uid_t euid = Int_val(e);
    uid_t suid = Int_val(s);

    if (setresuid(ruid, euid, suid) != 0)
        error(strerror(errno));
    CAMLreturn(Val_unit);
}

CAMLprim value
ocaml_setresgid(value r, value e, value s)
{
    CAMLparam3(r, e, s);
    gid_t rgid = Int_val(r);
    gid_t egid = Int_val(e);
    gid_t sgid = Int_val(s);

    if (setresgid(rgid, egid, sgid) == -1)
        error(strerror(errno));
    CAMLreturn(Val_unit);
}

static gid_t *
array_of_value(value v)
{
    CAMLparam1(v);
    gid_t *arr;
    mlsize_t size, i;

    size = Wosize_val(v);
    arr = caml_stat_alloc(size * sizeof(gid_t));
    for (i = 0; i < size; i++)
        arr[i] = Int_val(Field(v, i));

    CAMLreturnT (gid_t *, arr);
}

CAMLprim value
ocaml_setgroups(value group_list)
{
    CAMLparam1(group_list);
    gid_t *groups = array_of_value(group_list);

    if (setgroups(Wosize_val(group_list), groups) == -1)
        error(strerror(errno));
    CAMLreturn(Val_unit);
}
