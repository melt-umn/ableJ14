grammar edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;
import edu:umn:cs:melt:ableJ14:terminals;

nonterminal TypeRep  with  isSubTypeResult, isSubTypeOf_input,
                            isEqualTo, isEqualTo_input,
                            errors;


-- subtype_check(t1,t2) checks if t1 is a subtype of t2
function subtype_check
SubTypeRes ::= t1::TypeRep t2::TypeRep
{ return (decorate t1 with {isSubTypeOf_input = t2;}).isSubTypeResult ; }

function equality_check
Boolean ::= t1::TypeRep t2::TypeRep
{ return (decorate t1 with {isEqualTo_input = t2;}).isEqualTo ; }



-- convert to - convert an expresssion from one type to another
-- This function is expected to work, otherwise an error is thrown.
function convertTo
Expr ::= e::Expr orig_type::TypeRep  new_type::TypeRep
{
 return if   res.isSubType
        then converted_e
        else error ("Attempt to convert expression " ++ e.pp ++ " from type " ++ orig_type.pp ++
                    " to type " ++ new_type.pp ++ " failed.");

 local attribute res :: SubTypeRes ;
 res = subtype_check( orig_type, new_type );

 local attribute converting_e :: ConvertBy ;
 converting_e = res.convertBy ;
 converting_e.toConvertExpr = e ;

 local attribute converted_e :: Expr ;
 converted_e = converting_e.convertedExpr ;
}



-- Type Equality: checking if two TypeReps are the same type  --
----------------------------------------------------------------
synthesized attribute isEqualTo :: Boolean ;
inherited attribute isEqualTo_input :: TypeRep ;



-- Subtypes: checking if a TypeRep is a subtype of another --
-------------------------------------------------------------
-- To check that t1 <: t2, 
--  1. provide t2 as the inherited attribute isSubTypeOf_input to t1
--  2. use synthesized attribute isSubTypeResult on t1.

inherited attribute isSubTypeOf_input :: TypeRep ;

synthesized attribute isSubTypeResult :: SubTypeRes ;
-- the result of testing if a type is the subtype of another
-- type (which is provided as the inherited attribute isSubType_input


synthesized attribute isSubType_ConvertBy :: [ [ ConvertBy ] ] ;
attribute isSubType_ConvertBy occurs on TypeRep ;
-- Used in the implementation to collect lists of possible conversions.


synthesized attribute superTypes_ConvertBy :: [ ConvertBy ] with ++;--superTypes_ConvertBy_Collect ;
attribute superTypes_ConvertBy occurs on TypeRep ;
-- used by TypeReps to specify their super types and means
-- for converting to that specified super type.

--function superTypes_ConvertBy_Collect
--[ConvertBy] ::= l1::[ConvertBy] l2::[ConvertBy]
--{ return l1 ++ l2 ; }





-- SubTypeRes --
----------------
nonterminal SubTypeRes ; 
-- The SubTypeRes type stores the results of checking if one type is a 
-- subtype of another.  It has a number of synthesized attributes that
-- may be used by the construct testing for a subtype relationship
-- between types, say, t1, and, say, t2.

synthesized attribute isSubType :: Boolean ;
attribute isSubType occurs on SubTypeRes ;
-- a boolean reporting that t1 is a subtype of t2.

synthesized attribute convertBy :: ConvertBy ;
attribute convertBy occurs on SubTypeRes ;
-- the means by which the conversion is to be performed.
-- When t1 is a subclass of t2, then convertBy is the
-- identity conversion function.

synthesized attribute msg :: String ;
attribute msg occurs on SubTypeRes ;
-- A simple message about the conversion process.

attribute errors occurs on SubTypeRes ;
-- Errors may be detected when trying to perform subtype tests.  
-- If t1 is not a subtype of t2, that is NOT reported as an error
-- here - the construct accessing the SubTypeRes will create that
-- kind of error. 
-- The errors that may be recorded in this attribute are ones found
-- in the subtype relation itself.  For example, if there are more
-- than two ways to convert from t1 to t2 and no means for picking
-- one exists, then this host language will record that as an error
-- in this attribute.


-- a production to create a SubTypeRes
abstract production subtypeRes 
r::SubTypeRes ::= res::Boolean m::String err::[Error] cb::ConvertBy
{
 r.msg = m ;
 r.isSubType = res ;
 r.errors := err ;
 r.convertBy = cb ;
}



-- ConvertBy --
---------------
nonterminal ConvertBy with convertProd, superType, convertName, toConvertExpr, convertedExpr, msg;
-- A ConvertBy, that is associated with a TypeRep, specifies
-- how to convert the type to a (super) type.  These are stored
-- in the superTypes_ConvertBy list on a TypeRep.

-- A ConvertBy is also a component of the SubTypeRes tree that
-- is the result of a query about the subtype relation between 
-- two types.

-- To use a ConvertBy to convert an expression of one type to an 
-- expression of another, we give the original expression as the
-- inherited attribute toConvertExpr.  The synthesized attribute
-- convertedExpr is the converted expression that may have a 
-- different type.

synthesized attribute convertProd :: (Expr::=Expr) ;  -- maybe should be a local attr.
synthesized attribute superType   :: TypeRep ;  -- the type we are converting to
synthesized attribute convertName :: String ;
synthesized attribute convertedExpr :: Expr ;  
inherited attribute   toConvertExpr :: Expr ;


-- This is the generic "convert by" constructor.
abstract production mkConvertBy
c::ConvertBy ::= n::String p::(Expr::=Expr)  st::TypeRep 
{
 c.convertName = n ;
 c.convertProd = p ;
 c.superType = st ;
 c.convertedExpr = c.convertProd (c.toConvertExpr) ;
}


-- An "identity" converter.
abstract production mkNamedIdCnvt
c::ConvertBy ::= n::String st::TypeRep
{
 c.convertName = n ;
 c.convertProd = idConvertBy ;
 c.superType = st ;
 c.convertedExpr = c.toConvertExpr ;
}

abstract production mkIdCnvt
c::ConvertBy ::= st::TypeRep
{
 forwards to mkNamedIdCnvt("id",st);
}

abstract production mkInitCnvt
c::ConvertBy ::= st::TypeRep
{
 c.convertName = "InitCnvt" ;
 c.convertProd = idConvertBy ;
 c.superType = st ;
 c.convertedExpr = c.toConvertExpr ;
}

abstract production mkSubClassCnvt
c::ConvertBy ::= st::TypeRep
{
 c.convertName = "subclass convert to " ++  st.eqName ++ " ";
 c.convertProd = idConvertBy ;
 c.superType = st ;
 c.convertedExpr = c.toConvertExpr ;
}

abstract production mkSubClassOfObject
c::ConvertBy ::= t_env::[ ScopeEnv ]
{
 c.convertName = "subclass convert to Object " ;
 c.convertProd = idConvertBy ;
 c.superType = retrieveTypeRep ("java.lang.Object", t_env);
 c.convertedExpr = c.toConvertExpr ;
}

abstract production mkSubInterfaceCnvt
c::ConvertBy ::= st::TypeRep
{
 c.convertName = "sub-inteface convert to " ++  st.eqName ++ " ";
 c.convertProd = idConvertBy ;
 c.superType = st ;
 c.convertedExpr = c.toConvertExpr ;
}

abstract production mkToUnknownCnvt
c::ConvertBy ::=
{
 c.convertName = "convert to unknown type " ;
 c.convertProd = idConvertBy ;
 c.superType =  unknownTypeRep() ;
 c.convertedExpr = c.toConvertExpr ;
}

function isJavaHostTypeRep
Boolean ::= tr::TypeRep
{
 return true ;
}






-- Minimal Common Super Types                   --
--------------------------------------------------
-- The function minimal_common_super_types returns the list of
-- TypeRep's that super types of the parameters t1 and t2 and that are
-- not super types of other types (besides itself) in the list.

-- Several utility functions are defined further below that are used
-- here but may also be of use in other places.

function minimal_common_super_types
[ TypeRep ] ::= t1::TypeRep  t2::TypeRep
{
 -- 1. get super type of t1 and t2
 -- 2. get the intersection of the lists
 -- 3. keep only those that are not the super type of some other
 --    member of the list
 local attribute common_super_types :: [ TypeRep ] ;
 common_super_types = 
   map__get_typerep_ConvertBy(
            intersect_ConvertBy(all_super_types_ConvertBy(mkIdCnvt(t1)),
                                all_super_types_ConvertBy(mkIdCnvt(t2))) ) ;

 local attribute minimal_ones :: [ TypeRep ] ;
 minimal_ones = keep_minimal(common_super_types) ;

 return if length(minimal_ones) == 1
        then (if head(minimal_ones).isUnknownType
              then [ ]
              else minimal_ones)
        else minimal_ones ;
}

function keep_minimal  [TypeRep] ::= ts::[TypeRep]
{ return keep_minimal_helper(no_dups, no_dups) ;

  local attribute no_dups :: [ TypeRep ] ;
  no_dups = remove_duplicates_TypeRep (ts) ;
}

function keep_minimal_helper  [TypeRep] ::= ts::[TypeRep] all_ts::[TypeRep]
{  return if   null(ts)
          then [ ]
          else (if not_super_type_of( head(ts), all_ts)
                then [ head(ts) ]
                else [ ]
               ) ++ keep_minimal_helper ( tail(ts), all_ts ) ;
}


function all_super_types    
[ TypeRep ] ::= t::TypeRep
{
 return map__get_typerep_ConvertBy (all_super_types_ConvertBy(mkIdCnvt(t))) ;
}

function all_super_types_ConvertBy
[ ConvertBy ] ::= c::ConvertBy
{
 return c :: map__all_super_types_ConvertBy(sups) ;

 local attribute sups :: [ConvertBy] ;
 sups = c.superType.superTypes_ConvertBy ;
}

function map__all_super_types_ConvertBy
[ ConvertBy ] ::= cs :: [ ConvertBy ]
{
 return if   null(cs)
        then [ ]
        else all_super_types_ConvertBy(head(cs)) ++ map__all_super_types_ConvertBy(tail(cs)) ;
}

function map__get_typerep_ConvertBy
[ TypeRep ] ::= cs :: [ ConvertBy ]
{
 return if   null(cs)
        then [ ]
        else (head(cs)).superType :: map__get_typerep_ConvertBy(tail(cs)) ;
}

function intersect_ConvertBy
[ ConvertBy ] ::=  l1::[ConvertBy] l2::[ConvertBy]
{ -- select all elements of l1 that appear in l2
  return if   null(l1) 
         then [ ] 
         else (if   elem_of_ConvertBy( head(l1), l2) 
               then [ head(l1) ] 
               else [ ]
              ) ++    intersect_ConvertBy (tail(l1), l2) ;
}

function elem_of_ConvertBy
Boolean ::= c::ConvertBy  cs::[ConvertBy]
{
 return if   null(cs)
        then false
        else if equality_check(c.superType , (head(cs)).superType )
             then true
             else elem_of_ConvertBy(c, tail(cs)) ;
}


function intersect_TypeRep
[ TypeRep ] ::=  l1::[TypeRep] l2::[TypeRep]
{ -- select all elements of l1 that appear in l2
  return if   null(l1) 
         then [ ] 
         else (if   elem_of_TypeRep( head(l1), l2) 
               then [ head(l1) ] 
               else [ ]
              ) ++    intersect_TypeRep (tail(l1), l2) ;
}

function elem_of_TypeRep
Boolean ::= c::TypeRep  cs::[TypeRep]
{
 return if   null(cs)
        then false
        else if equality_check(c, head(cs) )
             then true
             else elem_of_TypeRep(c, tail(cs)) ;
}

function toString_TypeReps
String ::= cs::[TypeRep]
{
 return if   null(cs)
        then ""
        else ".... " ++ head(cs).pp ++ " ....\n" ++  toString_TypeReps(tail(cs)) ;
}



function not_super_type_of
Boolean ::= t::TypeRep  ts::[TypeRep]
{ -- if t is not a super type of any element of ts (except it self)
  -- then return true, else false
 return if   null(ts)
        then true
        else 
        if   equality_check(t, head(ts))
        then not_super_type_of(t, tail(ts))
        else
        if   (subtype_check(t, head(ts))).isSubType
        then not_super_type_of(t, tail(ts))
        else false  ;
}

function remove_duplicates_TypeRep
[TypeRep] ::= ts::[TypeRep]
{
 return if   null(ts)
        then ts
        else if   ! elem_of_TypeRep( head(ts), tail(ts) )
             then head(ts) :: remove_duplicates_TypeRep ( tail(ts) )
             else remove_duplicates_TypeRep ( tail(ts) ) ;
}



-- This is the abstract production for a language construct that lets
-- one test the miminal_common_super_type function.
-- ToDo: remove before distribution?

abstract production test_miminal_common_super_type
s::Stmt ::= id::Id_t  t1::Type  t2::Type
{
 forwards to erroneous_Stmt(s, [ mkError(0, emsg) ] ) ;

 local attribute emsg :: String ;
 emsg = if    id.lexeme == "mcst"
        then  "Mimimal common super types of " ++ t1.pp ++ " and " ++ t2.pp ++ " are\n"  ++
              toString_TypeReps( mins ) ++ "\n" 

        else  "id in mimimal common super type test not recognized." ;

 local attribute mins::[TypeRep] ;
 mins = minimal_common_super_types (t1.typerep, t2.typerep);


 s.neededImportedSingleTypes = t1.neededImportedSingleTypes ++ t2.neededImportedSingleTypes;
 s.neededCurrentPackageTypes = t1.neededCurrentPackageTypes ++ t2.neededCurrentPackageTypes;
 s.neededImportedOnDemandTypes = t1.neededImportedOnDemandTypes ++ t2.neededImportedOnDemandTypes;
 s.neededFullyQualifiedTypes = t1.neededFullyQualifiedTypes ++ t2.neededFullyQualifiedTypes;

}
