use crate::*;

// user-facing
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub(crate) struct TypeId(pub u32);

pub(crate) struct ProdDef {
    pub name: &'static str,
    pub maybe_fields: Option<Vec<TypeId>>,
}

#[derive(Debug)]
pub(crate) struct TypeMap {
    // invariant: no primitive keys
    map_prod_defs: HashMap<TypeId, MapProdDef>,
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum TidSizeErr {
    RecursiveDepthExceeded,
    DependsOnSizeOfUnknown(TypeId),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum SizeLookupErr {
    UnknownTid,
    UnknownFields,
    UnknownSize,
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum MaybeFieldsLookupErr {
    PrimType,
    UnknownTid,
    UnknownFields,
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum NameLookupErr {
    UnknownTid,
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum NewErr {
    DefiningPrimTid { tid: TypeId },
}

// internal

struct PrimDef {
    name: &'static str,
    size: usize,
}

#[derive(Debug)]
struct MapProdDef {
    // we can have name without fields
    name: &'static str,
    maybe_fields_maybe_size: Option<FieldsMaybeSize>,
}
#[derive(Debug)]
struct FieldsMaybeSize {
    // we can have fields without size
    fields: Vec<TypeId>,
    maybe_size: Option<usize>,
}

//////////////////////////////

impl std::fmt::Debug for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_fmt(format_args!("tid{}", self.0))
    }
}

impl TypeId {
    pub(crate) const U08: Self = Self(0);
    pub(crate) const U32: Self = Self(1);
    // pub(crate) const TAG: Self = Self(2);
    const fn prim_def(self) -> Option<PrimDef> {
        Some(match self {
            Self::U08 => PrimDef { size: 1, name: "u08" },
            Self::U32 => PrimDef { size: 4, name: "u32" },
            // Self::TAG => PrimDef { size: 8, name: "tag" },
            _ => return None,
        })
    }
}
impl PrimDef {
    fn size(self) -> usize {
        self.size
    }
    fn name(self) -> &'static str {
        self.name
    }
}
impl TypeMap {
    pub(crate) fn type_is_readable(&self, tid: TypeId) -> bool {
        tid.prim_def().is_some() || self.lookup_type_size(tid).is_ok()
    }
    pub(crate) fn type_is_writable(&self, tid: TypeId) -> bool {
        self.lookup_type_size(tid).is_ok()
    }
    pub(crate) fn lookup_type_size(&self, tid: TypeId) -> Result<usize, SizeLookupErr> {
        if let Some(PrimDef { size, .. }) = tid.prim_def() {
            Ok(size)
        } else {
            self.map_prod_defs
                .get(&tid)
                .ok_or(SizeLookupErr::UnknownTid)?
                .maybe_fields_maybe_size
                .as_ref()
                .ok_or(SizeLookupErr::UnknownFields)?
                .maybe_size
                .ok_or(SizeLookupErr::UnknownSize)
        }
    }
    pub(crate) fn lookup_maybe_fields(
        &self,
        tid: TypeId,
    ) -> Result<Option<&Vec<TypeId>>, MaybeFieldsLookupErr> {
        if let Some(_) = tid.prim_def() {
            Err(MaybeFieldsLookupErr::PrimType)
        } else {
            Ok(self
                .map_prod_defs
                .get(&tid)
                .ok_or(MaybeFieldsLookupErr::UnknownTid)?
                .maybe_fields_maybe_size
                .as_ref()
                .map(|fields_size| &fields_size.fields))
        }
    }
    pub(crate) fn lookup_type_name(&self, tid: TypeId) -> Result<&'static str, NameLookupErr> {
        if let Some(PrimDef { name, .. }) = tid.prim_def() {
            Ok(name)
        } else {
            self.map_prod_defs
                .get(&tid)
                .map(|map_prod_def| map_prod_def.name)
                .ok_or(NameLookupErr::UnknownTid)
        }
    }
    pub(crate) fn new(prod_defs: HashMap<TypeId, ProdDef>) -> Result<Self, NewErr> {
        if let Some(tid) = prod_defs.keys().copied().filter(|tid| tid.prim_def().is_some()).next() {
            return Err(NewErr::DefiningPrimTid { tid });
        }
        // invariant established: no prim TypeId keys
        let mut me = Self {
            map_prod_defs: prod_defs
                .into_iter()
                .map(|(tid, ProdDef { name, maybe_fields })| {
                    let map_prod_def = MapProdDef {
                        name,
                        maybe_fields_maybe_size: maybe_fields
                            .map(|fields| FieldsMaybeSize { fields, maybe_size: None }),
                    };
                    (tid, map_prod_def)
                })
                .collect(),
        };
        // compute product sizes
        let mut tid_with_size_todo: Vec<_> = me.map_prod_defs.keys().copied().collect();
        let mut progress = true;
        while progress {
            progress = false;
            tid_with_size_todo.retain(|tid| {
                if let Some(fields_maybe_size) =
                    me.map_prod_defs.get(tid).unwrap().maybe_fields_maybe_size.as_ref()
                {
                    // attempt to compute size now
                    let maybe_size =
                        fields_maybe_size.fields.iter().fold(Some(0), |acc, &field_tid| {
                            Some(acc? + me.lookup_type_size(field_tid).ok()?)
                        });
                    if maybe_size.is_some() {
                        // "upgrade" ref to mut ref
                        let maybe_fields_maybe_size = me
                            .map_prod_defs
                            .get_mut(tid)
                            .unwrap()
                            .maybe_fields_maybe_size
                            .as_mut()
                            .unwrap();
                        // update size, prolong the loop, don't retain this TID.
                        maybe_fields_maybe_size.maybe_size = maybe_size;
                        // assert!(
                        //     maybe_size != Some(0),
                        //     "implementation cannot handle no-data prods"
                        // );
                        progress = true;
                        false
                    } else {
                        true
                    }
                } else {
                    // this prod's size will stay unknown. stop considering it.
                    false
                }
            });
        }
        Ok(me)
    }
}
