/**************************************************************************/
/*                                                                        */
/*    Copyright 2020 David Allsopp Ltd.                                   */
/*                                                                        */
/*  All rights reserved. This file is distributed under the terms of the  */
/*  GNU Lesser General Public License version 2.1, with the special       */
/*  exception on linking described in the file LICENSE.                   */
/*                                                                        */
/**************************************************************************/

#include <unistd.h>
#include <sys/types.h>
#include <acl/libacl.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#ifndef Val_none
#define Val_none Val_int(0)
#endif

/* OPAM_get_acl_executable_info(file, owner) takes a filename and the uid of
 * the file's owner (this saves a call to stat on both the OCaml and C sides).
 * The result is:
 *   None - the process cannot execute file
 *   Some [] - the process can execute file
 *   Some gids - the process can execute file if it is any of these gids
 */
CAMLprim value OPAM_get_acl_executable_info(value file, value owner)
{
  CAMLparam2(file, owner);
  CAMLlocal2(result, cell);
  acl_t acl = acl_get_file(String_val(file), ACL_TYPE_ACCESS);
  uid_t owner_uid = Int_val(owner);
  uid_t uid = geteuid();

  result = Val_none;

  if (acl)
  {
    acl_entry_t entry;

    if (acl_get_entry(acl, ACL_FIRST_ENTRY, &entry) == 1)
    {
      int mask = 1;
      int user = 0;
      do
      {
        acl_tag_t tag;
        acl_permset_t perms;
        if (acl_get_tag_type(entry, &tag) == 0 &&
            acl_get_permset(entry, &perms) == 0)
        {
          void *qualifier = NULL;
          int executable = acl_get_perm(perms, ACL_EXECUTE);

          switch(tag)
          {
            case ACL_USER:
              if (executable && (qualifier = acl_get_qualifier(entry)))
              {
                uid_t entry_uid = *((uid_t *)qualifier);
                /* NB ACL_USER entries do not override ACL_USER_OBJ */
                if (entry_uid != owner_uid && entry_uid == uid)
                {
                  /* result = Some [] */
                  if (!Is_block(result))
                  {
                    result = caml_alloc_small(1, 0);
                    Field(result, 0) = Val_int(0);
                  } else {
                    caml_modify(&Field(result, 0), Val_int(0));
                  }
                  user = 1;
                }
              }
              break;
            case ACL_GROUP:
              if (!user && executable && (qualifier = acl_get_qualifier(entry)))
              {
                gid_t entry_gid = *((gid_t *)qualifier);
                /* Construct a cons cell */
                cell = caml_alloc_small(2, 0);
                Field(cell, 0) = Val_int(entry_gid);
                if (Is_block(result))
                {
                  /* Put cons cell at head of existing list */
                  Field(cell, 1) = Field(result, 0);
                  caml_modify(&Field(result, 0), cell);
                }
                else
                {
                  /* result = Some [cell] */
                  Field(cell, 1) = Val_int(0);
                  result = caml_alloc_small(1, 0);
                  Field(result, 0) = cell;
                }
              }
              break;
            case ACL_USER_OBJ:
            case ACL_GROUP_OBJ:
              /* These have already been done by the stat check */
              break;
            case ACL_MASK:
              if (!(mask = executable))
              {
                result = Val_none;
              }
              break;
            default:
              /* ACL_UNDEFINED_TAG or ACL_OTHER */
              break;
          }

          if (qualifier)
            acl_free(qualifier);
        }
      } while (mask && acl_get_entry(acl, ACL_NEXT_ENTRY, &entry) == 1);
    }

    acl_free((void *)acl);
  }

  CAMLreturn(result);
}
