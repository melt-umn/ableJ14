grammar edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;

------------------------------------------------------------
-- Helper Functions for subtype                           --
------------------------------------------------------------

function mkSubTypeResult
SubTypeRes ::= conversions:: [ [ ConvertBy ] ]
{
 local attribute num_conversions :: Integer ;
 num_conversions = length (conversions);


 local attribute mm::String ;
 mm = "Number of conversions: " ++ toString(length(conversions)) ++ "\n   " ++
      toString_ConvertBy_List_List(conversions) ;

 return if null (sorted_conversions)
	then subtypeRes( false, mm, errs, convertByError ())
	else subtypeRes( num_conversions > 0, mm,  errs,   collapseConvertByList(head(sorted_conversions)) ) ;

 local attribute errs :: [Error] ;
 errs = if null (sorted_conversions)
         then []   -- if a subtype relation does not exist and that is the
                    -- cause of an error, then that error is generated by the
                    -- function calling mkSubTypeResult.
                    -- Errors here are for errors in the type system - e.g. more
                    -- than one "shortest" conversions between two types.

        else if length(sorted_conversions) == 1
         then []   -- exactly one conversion, so no errors

        else if length( head(sorted_conversions) ) ==
                length( head(tail(sorted_conversions)) )
         then -- there are more than 1 conversion of the shorted length.
              [ mkError (-1, "Error - more than one \"shortest\" conversion.\n" ++ mm )] 
         else [] ;

 local attribute sorted_conversions :: [[ConvertBy]] ;
 sorted_conversions = cbMergeSort(conversions);
}


function toString_ConvertBy_List_List
String ::= cnvts::[[ConvertBy]]
{
 return if null(cnvts)
        then ""
        else toString_ConvertBy_List(head(cnvts)) ++ "\n    " ++ toString_ConvertBy_List_List(tail(cnvts)) ;
}

function toString_ConvertBy_List
String ::= cnvts::[ConvertBy]
{
 return if null(cnvts)
        then ""
        else head(cnvts).superType.pp ++ " with cnvter "   ++ head(cnvts).convertName ++ "  ==>  " ++ 
             toString_ConvertBy_List(tail(cnvts)) ;
}


-- TypeRep productions must define these attributes as they are used in
-- the _implementation_ of the subtype test.  "Users" of the test
-- don't need to know about these, only those in TypeRep_API.sv

synthesized attribute isSubType_ConvertBy_Accum :: [ [ConvertBy] ] ;
inherited attribute isSubType_ConvertBy_Accum_currCnvtBy :: ConvertBy ;
--inherited attribute isSubType_ConvertBy_Accum_input :: TypeRep ;
inherited attribute isSubType_ConvertBy_Accum_sofar :: [ConvertBy] ;

attribute isSubType_ConvertBy_Accum occurs on TypeRep ;
attribute isSubType_ConvertBy_Accum_currCnvtBy occurs on TypeRep ;
--attribute isSubType_ConvertBy_Accum_input occurs on TypeRep ;
attribute isSubType_ConvertBy_Accum_sofar occurs on TypeRep ;

function searchSuperTypes_ConvertBy_checkCirc
[[ ConvertBy ]] ::= sts:: [ConvertBy]  t2::TypeRep  checked_so_far::[ConvertBy]
{
 return if null(sts) 
        then []
        else head_sts_converts_to_t2  ++ 
             searchSuperTypes_ConvertBy_checkCirc ( tail(sts), t2, checked_so_far) ;

 local attribute head_sts_converts_to_t2 :: [ [ ConvertBy ] ] ;
 head_sts_converts_to_t2 = ( decorate head(sts).superType 
                             with { isSubType_ConvertBy_Accum_currCnvtBy = head(sts);
                                    isSubTypeOf_input = t2;
                                    isSubType_ConvertBy_Accum_sofar = checked_so_far ;
                                  } ) . isSubType_ConvertBy_Accum ;
}



-- the "standard" subtype/convert-by check
-- t1 <: t2  if t1 == t2 return identity conversion to t2
--           else searchSuperTypes_ConvertBy_checkCirc ( t1.superTypes_ConvertBy, t2 )
function standard_convertby_check_sofar
[[ ConvertBy]] ::= t1_cnvt::ConvertBy t2::TypeRep sofar::[ConvertBy]
{
 return if t1_isEqual_t2 
        then [ [ t1_cnvt ] ]
        else --  [] ++    perhaps, to also return the results from below...
             mapCons_ConvertBy ( t1_cnvt, rest ) ;

 local attribute rest :: [[ConvertBy]];
 rest = searchSuperTypes_ConvertBy_checkCirc ( setminus_ConvertBy (supertypeList, [t1_cnvt]++sofar ) , 
                                               t2, 
                                               [t1_cnvt] ++ sofar ) ;

 local attribute supertypeList :: [ ConvertBy ] ;
 supertypeList = t1_cnvt.superType.superTypes_ConvertBy ;

 local attribute t1_isEqual_t2 :: Boolean ;
 t1_isEqual_t2 = (decorate t1_cnvt.superType with {isEqualTo_input=t2;}) . isEqualTo ;
}



function mapCons_ConvertBy
[[ConvertBy]] ::= hd::ConvertBy tails::[[ConvertBy]]
{
 return if null(tails)
        then []
        else (hd :: head(tails) ) ::
             mapCons_ConvertBy (hd, tail(tails)) ;
}

function setminus_ConvertBy 
[ConvertBy] ::= set1::[ConvertBy] set2::[ConvertBy]
{  -- set1 \ set2
   -- if x in set1 and x not in set2, then x in result

 return  if null(set1)
         then []
         else if ! ( elem_ConvertBy (head(set1),set2)) 
              then head(set1) :: rest
              else rest ;
 local attribute rest :: [ConvertBy];
 rest = setminus_ConvertBy ( tail(set1), set2) ;
}

function elem_ConvertBy
Boolean ::= e::ConvertBy l::[ConvertBy]
{ -- check if e is an element of l
 return if null(l)
        then false
        else e_equals_head_l || elem_ConvertBy(e,tail(l)) ;

 local attribute e_equals_head_l :: Boolean ;
 e_equals_head_l = (decorate e.superType with {isEqualTo_input= head(l).superType ;}) . isEqualTo 
                   &&
                   e.convertName == head(l).convertName ;
}


-- not in the interface?
--synthesized attribute isSubType_ConvertBy :: [ [ ConvertBy ] ] ;
--attribute isSubType_ConvertBy occurs on TypeRep ;



abstract production idConvertBy
e::Expr ::= e2::Expr 
{ -- e.pp = "idConvertBy(" ++ e2.pp ++ ")" ;
  forwards to e2 ; 
}







-- collapse ConvertBy List --
-----------------------------
function collapseConvertByList
ConvertBy ::= cbs::[ConvertBy]
{ return if null(cbs)
         then convertByNone()
         else convertByCons( head(cbs), collapseConvertByList(tail(cbs))) ;
}

abstract production convertByNone
cbs::ConvertBy ::= 
{ cbs.convertedExpr = cbs.toConvertExpr ;
  cbs.msg = "" ; 
}

abstract production convertByOne
cbs::ConvertBy ::= cb::ConvertBy
{ cbs.convertedExpr = cb.convertProd ( cbs.toConvertExpr ) ;
  cbs.msg = cb.msg ; 
}

abstract production convertByCons
cbs::ConvertBy ::= cb::ConvertBy cbstail::ConvertBy
{ cbs.convertedExpr = cb.convertProd ( cbstail.convertedExpr ) ;
  cbstail.toConvertExpr = cbs.toConvertExpr ; 
  cbs.msg = cb.msg ++ "..." ++ cbstail.msg ;
}

abstract production convertByError
cbs::ConvertBy ::= 
{ cbs.convertedExpr = errorExpr ( cbs.toConvertExpr ) ;
  cbs.msg = "Convert error"; 
}

abstract production errorExpr
e::Expr ::= expr::Expr
{ e.pp = "Error converting " ++ expr.pp ++ ".";
  e.basepp = "Error converting " ++ expr.basepp ++ ".";
  e.errors := [mkError (-1,  "Error converting " ++ expr.basepp)];
  e.typerep = expr.typerep;
}

-- Merge Sort for [[ ConvertBy ]] --
------------------------------------
function cbMergeSort
[ [ConvertBy] ] ::= c1::[ [ConvertBy] ]
{
  return cbMergeSortHelp(mapConvertBy(c1));
}

function cbMergeSortHelp
[ [ConvertBy] ] ::= c1::[[ [ConvertBy] ]]
{
  return if null(c1) then []
         else if null(tail(c1)) then head(c1)
         else cbMergeSortHelp(cbMergePairs(c1));
}

function cbMergePairs
[[ [ConvertBy] ]] ::= c1::[[ [ConvertBy] ]]
{
  return if null(c1) then []
         else if null(tail(c1)) then c1
         else cons(mergeConvertBy(head(c1), head(tail(c1))), cbMergePairs(tail(tail(c1))));
}

function mergeConvertBy
[ [ConvertBy] ] ::= tds1::[ [ConvertBy] ] tds2::[ [ConvertBy] ]
{
  return if null(tds2)
         then tds1
         else if null(tds1)
         then tds2
         else if  -- head(tds1).lexerPrec  <  head(tds2).lexerPrec
                  length(head(tds1))  <  length(head(tds2))
              then cons( head(tds2) , merge_tds1_tds2_tail )
              else cons( head(tds1) , merge_tds1_tail_tds2 ) ;

  local attribute merge_tds1_tds2_tail :: [ [ConvertBy] ] ;
  merge_tds1_tds2_tail = mergeConvertBy( tds1, tail(tds2) ) ;

  local attribute merge_tds1_tail_tds2 :: [ [ConvertBy] ] ;
  merge_tds1_tail_tds2 = mergeConvertBy( tail(tds1), tds2 ) ;

}

function mapConvertBy
[[ [ConvertBy] ]] ::= tds1::[ [ConvertBy] ]
{
  return if null(tds1)
         then []
         else cons([head(tds1)], mapConvertBy(tail(tds1)));
}
