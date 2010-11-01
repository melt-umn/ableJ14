grammar edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;
import edu:umn:cs:melt:ableJ14:abstractsyntax:packages;
import edu:umn:cs:melt:ableJ14:terminals;
import core;

-- As stated in 6.5, names are initially categorized syntactically and
-- the   reclassified   based   on   semantics  (variable   and   type
-- declarations).  The  constructs in  the file implement  the initial
-- syntactic classification and semantic reclassification.  Forwarding
-- provides  a   very  convenient  mechanism   for  implementing  this
-- reclassification.

--------------------------------------------------
-- 6.5, 6.5.1 Syntactic Classification:
--------------------------------------------------
-- Initially names  are classified  as one of  the following  kinds of
-- names:

nonterminal PackageName;
nonterminal TypeName;
nonterminal ExprName;
nonterminal MethodName;
nonterminal PackageOrTypeName;
nonterminal AmbiguousName;

-- Names  initially classified  as AmbiguousName  or PackageOrTypeName
-- will be reclassified as PackageName, TypeName, or ExprName.

-- The syntactic classification described  in 6.5.1 is accomplished by
-- having  the  concrete  productions  (which  use  a  general  Name_C
-- nonterminal) create  abstract trees  using one of  the nonterminals
-- above. This avoids parser conflicts.

-- The grammar given in 6.5 for various kinds of names is given below:
-- 
-- PackageName:
--	Identifier
--	PackageName . Identifier
--
-- TypeName:
--	Identifier
--	PackageOrTypeName . Identifier
--
-- ExpressionName:
--	Identifier
--	AmbiguousName . Identifier
--
-- MethodName:
--	Identifier
--	AmbiguousName . Identifier
--
-- PackageOrTypeName:
--	Identifier
--	PackageOrTypeName . Identifier
--
-- AmbiguousName:
--	Identifier
--	AmbiguousName . Identifier


--------------------------------------------------
-- DisambiguatedName
--------------------------------------------------
synthesized attribute disambiguatedName :: DisambiguatedName;
nonterminal DisambiguatedName;

abstract production disambiguated_expr_name
dn::DisambiguatedName ::= e::LHS {
}

abstract production disambiguated_type_name
dn::DisambiguatedName ::= tr::TypeRep {
}

abstract production disambiguated_method_name
dn::DisambiguatedName ::= m_reps::[ MethodDclRep ] {
}

abstract production disambiguated_package_name
dn::DisambiguatedName ::= pn::PackageName {
}

abstract production disambiguated_error_name
dn::DisambiguatedName ::= errs::[ Error ] {
}

--------------------------------------------------
-- 6.5.3 Meaning of Package Names
--------------------------------------------------

attribute pp, basepp occurs on PackageName;

abstract production simple_package_name
pn::PackageName ::= id::Id_t {
 -- 6.5.3.1

 pn.pp = id.lexeme ;
 pn.basepp = id.lexeme ;
}

abstract production qualified_package_name
pn::PackageName ::= pn2::PackageName id::Id_t {
 -- 6.5.3.2

 pn.pp = pn2.pp ++ "." ++ id.lexeme;
 pn.basepp = pn2.basepp ++ "." ++ id.lexeme;
}

--------------------------------------------------
-- 6.5.4 Meaning of Package Or Type Names
--------------------------------------------------

attribute pp, basepp, type_env, disambiguatedName occurs on PackageOrTypeName;

abstract production simple_package_or_type_name 
ptn::PackageOrTypeName ::= id::Id_t {
 -- 6.5.4.1 
 production attribute resolvedType :: TypeSearchResult;
 production attribute resolvedPackageOrType :: PackageOrTypeSearchResult;

 ptn.pp = id.lexeme ;
 ptn.basepp = id.lexeme ;

 ptn.disambiguatedName = case ptn.resolvedPackageOrTypeName of
				fully_qualified_name_none () -> disambiguated_error_name (resolvedPackageOrType.errors) |
				fully_qualified_name_unknown () -> disambiguated_error_name (resolvedPackageOrType.errors) |
				fqn -> if ptn.isPackage
					then disambiguated_package_name (simple_package_name (id))
					else disambiguated_type_name (retrieveTypeRep2 (fqn.qualifiedName, ptn.type_env, ptn.file_name, id.line))
			 end;
 }

abstract production qualified_package_or_type_name
ptn::PackageOrTypeName ::= pn::PackageOrTypeName id::Id_t {
 -- 6.5.4.2
 production attribute resolvedType :: TypeSearchResult;
 production attribute resolvedPackageOrType :: PackageOrTypeSearchResult;

 ptn.pp = pn.pp ++ "." ++ id.lexeme;
 ptn.basepp = pn.basepp ++ "." ++ id.lexeme;

 local attribute q :: DisambiguatedName;
 q = pn.disambiguatedName;

 local attribute typesToSearch :: [ ScopeEnv ];
 typesToSearch = case q'' of
                 disambiguated_type_name (t) ->
                    case t of
                            classTypeRep (ctr) -> ctr.innerTypes |
                            interfaceTypeRep (itr) -> itr.innerTypes |
                            _ -> error ("Internal compiler error 1 production qualified_package_or_type_name " ++ ptn.pp)
		     end |
                 _ ->
                    error ("Internal compiler error 2 production qualified_package_or_type_name " ++ ptn.pp)
                 end;

 local attribute typeSearchResult :: [ DclInfo ];
 typeSearchResult = lookupId (id, typesToSearch);

 local attribute firstType :: TypeRep;
 firstType = case (head (typeSearchResult)).dclrep of
                  dcl_rep_class (class_dcl_rep (_, t)) -> t'' |
                  dcl_rep_interface (interface_dcl_rep (_, t)) -> t'' |
                  _ -> error ("Internal compiler error 3 production qualified_package_or_type_name " ++ ptn.pp)
              end;

 ptn.disambiguatedName =
 case q'' of
 disambiguated_package_name (p) ->
	    case ptn.resolvedPackageOrTypeName of
		fully_qualified_name_unknown () -> 
                    disambiguated_error_name ([mkError (id.line, "Unknown member " ++ id.lexeme ++ " in package " ++ pn.pp) ]) |
		fully_qualified_name_none () -> 
                    disambiguated_error_name ([mkError (id.line, "Unknown member " ++ id.lexeme ++ " in package " ++ pn.pp) ]) |
		fqn -> if ptn.isPackage
			then disambiguated_package_name (qualified_package_name (p, id))
			else disambiguated_type_name (retrieveTypeRep2 (fqn.qualifiedName, ptn.type_env, ptn.file_name, id.line))
	     end |

 disambiguated_type_name (t) ->
             case t of
	      unknownTypeRep () ->
	        disambiguated_type_name (unknownTypeRep ()) |

	      errorTypeRep (errs) ->
	        disambiguated_error_name (errs) |

	      classTypeRep (ctr) ->
	       if !null (typeSearchResult)
		then (if length (typeSearchResult) > 1
		       then disambiguated_error_name ([mkError (id.line, "Class " ++ ctr.name ++ " has multiple member types " ++ id.lexeme) ])

			-- check access

		       else disambiguated_type_name (firstType))

		else if hasUnknownSupers (t)
			 then disambiguated_type_name (unknownTypeRep ())

		else disambiguated_error_name ([mkError (id.line, "Class " ++ ctr.name ++ " does not have member " ++ id.lexeme) ]) |

	      interfaceTypeRep (itr) ->
		if !null (typeSearchResult)
		then (if length (typeSearchResult) > 1
		        then disambiguated_error_name ([mkError (id.line, "Interface " ++ itr.name ++ " has multiple member types " ++ id.lexeme) ])

			-- check access

		        else disambiguated_type_name (firstType))

		else if hasUnknownSupers (t)
			 then disambiguated_type_name (unknownTypeRep ())

		else disambiguated_error_name ([mkError (id.line, "Interface " ++ itr.name ++ " does not have member " ++ id.lexeme) ]) |

	       _ -> 
		    disambiguated_error_name ([mkError (id.line, "Type that is not a class or interface in PackageOrTypeName " ++ ptn.pp)])
	    end |

 disambiguated_error_name (errs) ->
            disambiguated_error_name (errs) |

 _ ->
            error ("Internal compiler error 4 in production qualified_package_or_type_name " ++ ptn.pp)
 end;
}

--------------------------------------------------
-- 6.5.5 Meaning of Type Names
--------------------------------------------------

attribute pp, basepp, disambiguatedName, type_env occurs on TypeName;

abstract production simple_type_name
tn::TypeName ::= id::Id_t {
 -- 6.5.5.1
 production attribute resolvedType :: TypeSearchResult;
 production attribute resolvedPackageOrType :: PackageOrTypeSearchResult;

 tn.pp = id.lexeme ;
 tn.basepp = id.lexeme ;

 tn.disambiguatedName = case tn.resolvedPackageOrTypeName of
                           fully_qualified_name_none () -> disambiguated_error_name (resolvedType.errors) |
                           fully_qualified_name_unknown () -> disambiguated_error_name (resolvedType.errors) |
                           fqn -> disambiguated_type_name (retrieveTypeRep2 (fqn.qualifiedName, tn.type_env, tn.file_name, id.line))
	                end; 
}

abstract production qualified_type_name
tn::TypeName ::= pn::PackageOrTypeName id::Id_t {
 -- 6.5.4.2
 production attribute resolvedPackageType :: PackageOrTypeSearchResult;
 production attribute resolvedPackageOrType :: PackageOrTypeSearchResult;

 tn.pp = pn.pp ++ "." ++ id.lexeme;
 tn.basepp = pn.basepp ++ "." ++ id.lexeme;

 local attribute q :: DisambiguatedName;
 q = pn.disambiguatedName;

 local attribute typesToSearch :: [ ScopeEnv ];
 typesToSearch = case q'' of
                 disambiguated_type_name (t) ->
                    case t of
                            classTypeRep (ctr) -> ctr.innerTypes |
                            interfaceTypeRep (itr) -> itr.innerTypes |
                            _ -> error ("Internal compiler error 1 production qualified_type_name " ++ tn.pp)
		    end |
                 _ ->
                    error ("Internal compiler error 2 production qualified_type_name " ++ tn.pp)
                 end;

 local attribute typeSearchResult :: [ DclInfo ];
 typeSearchResult = lookupId (id, typesToSearch);

 local attribute firstType :: TypeRep;
 firstType = case (head (typeSearchResult)).dclrep of
                  dcl_rep_class (class_dcl_rep (_, t)) -> t'' |
                  dcl_rep_interface (interface_dcl_rep (_, t)) -> t'' |
                  _ -> error ("Internal compiler error 3 production qualified_type_name " ++ tn.pp)
              end;

 tn.disambiguatedName =
 case q'' of
 disambiguated_package_name (p) ->
	     case tn.resolvedPackageOrTypeName of
		fully_qualified_name_unknown () -> disambiguated_error_name (resolvedPackageOrType.errors) |
		fully_qualified_name_none () -> disambiguated_error_name (resolvedPackageOrType.errors) |
		fqn -> if tn.isPackage
			then error ("Internal compiler error 4 production qualified_type_name " ++ tn.pp)
			else disambiguated_type_name (retrieveTypeRep2 (fqn.qualifiedName, tn.type_env, tn.file_name, id.line))
	     end |

 disambiguated_type_name (t) ->
             case t of
	      unknownTypeRep () ->
	        disambiguated_type_name (unknownTypeRep ()) |

	      errorTypeRep (errs) ->
	        disambiguated_error_name (errs) |

	      classTypeRep (ctr) ->
	       if !null (typeSearchResult)
		then (if length (typeSearchResult) > 1
		       then disambiguated_error_name ([mkError (id.line, "Class " ++ ctr.name ++ " has multiple member types " ++ id.lexeme) ])

			-- check access

		       else disambiguated_type_name (firstType))

		else if hasUnknownSupers (t)
			 then disambiguated_type_name (unknownTypeRep ())

		else disambiguated_error_name ([mkError (id.line, "Class " ++ ctr.name ++ " does not have member " ++ id.lexeme) ]) |

	      interfaceTypeRep (itr) ->
		if !null (typeSearchResult)
		then (if length (typeSearchResult) > 1
		        then disambiguated_error_name ([mkError (id.line, "Interface " ++ itr.name ++ " has multiple member types " ++ id.lexeme) ])

			-- check access

		        else disambiguated_type_name (firstType))

		else if hasUnknownSupers (t)
			 then disambiguated_type_name (unknownTypeRep ())

		else disambiguated_error_name ([mkError (id.line, "Interface " ++ itr.name ++ " does not have member " ++ id.lexeme) ]) |

	       _ -> 
		    disambiguated_error_name ([mkError (id.line, "Type that is not a class or interface in TypeName " ++ tn.pp)])
	    end |

 disambiguated_error_name (errs) ->
            disambiguated_error_name (errs) |

 _ ->
            error ("Internal compiler error 5 in production qualified_type_name " ++ tn.pp)
 end;
}

--------------------------------------------------
-- 6.5.6 Meaning of Expression Names
--------------------------------------------------

attribute pp, basepp, disambiguatedName, env, type_env, enclosingType occurs on ExprName;

-- Section 6.5.6

-- The following production handles cases in 6.5.6.1
abstract production simple_expr_name
en::ExprName ::= id::Id_t {
 en.pp = id.lexeme;
 en.basepp = id.lexeme;

 -- add stuff for instance vs static checking
 -- if final, forward to a "value_id" production

 local attribute idSearchResult :: [ DclInfo ] ;
 idSearchResult = lookupIdOneScope (id, en.env ++ (case en.enclosingType of
						   classTypeRep (ctr) -> ctr.fields |
						   interfaceTypeRep (itr) ->  itr.fields |
						   _ -> error ("Internal compiler error 1 in file " ++ en.file_name ++ ":" ++ toString (id.line) ++ 
									" in production simple_expr_name " ++ en.pp)
						   end));

 production attribute firstId :: DclRep ;
 firstId = (head (idSearchResult)).dclrep ;

 en.disambiguatedName = if null (idSearchResult)
                             	then disambiguated_error_name ([mkError (id.line, "Unknown identifier " ++ id.lexeme)])
	                else if length (idSearchResult) > 1
                             	then disambiguated_error_name ([mkError (id.line, "Multiple definitions for identifier " ++ id.lexeme ++ " in scope")])
			else if firstId.is_local
				then disambiguated_expr_name (bound_id (id, firstId))
			else if firstId.is_param
				then disambiguated_expr_name (bound_id (id, firstId))
			else if firstId.is_field
				then (if firstId.field_rep.is_static
					then disambiguated_expr_name (disambiguated_static_field_access (en.enclosingType, id, firstId.field_rep))
					else disambiguated_expr_name (simple_field_access (id, firstId.field_rep))
		     	     	     )
			else error ("Internal compiler error 2 in production simple_expr_name: " ++ en.pp);
}

abstract production qualified_expr_name
en::ExprName ::= an::AmbiguousName  id::Id_t {

 en.pp = an.pp ++ "." ++ id.lexeme;
 en.basepp = an.basepp ++ "." ++ id.lexeme;

 -- Apply the rules from 6.5.6.2

 local attribute q :: DisambiguatedName;
 q = an.disambiguatedName;

 local attribute fieldsToSearch :: [ ScopeEnv ];
 fieldsToSearch = case q'' of
                  disambiguated_type_name (t) ->
                         case t of
                            classTypeRep (ctr) -> ctr.fields |
                            interfaceTypeRep (itr) -> itr.fields |
                            _ -> error ("Internal compiler error 1 production qualified_expr_name " ++ en.pp)
		         end |

		  disambiguated_expr_name (l) ->
                          case l.typerep of
                            classTypeRep (ctr) -> ctr.fields |
                            interfaceTypeRep (itr) -> itr.fields |
                            _ -> error ("Internal compiler error 2 production qualified_expr_name " ++ en.pp)
		          end |
                  _ ->
                          error ("Internal compiler error 3 production qualified_expr_name " ++ en.pp)
                  end;

 local attribute fieldSearchResult :: [ DclInfo ];
 fieldSearchResult = lookupId (id, fieldsToSearch);

 local attribute firstField :: FieldDclRep;
 firstField = case (head (fieldSearchResult)).dclrep of
                  dcl_rep_field (fdr) -> fdr'' |
                  _ -> error ("Internal compiler error 4 production qualified_expr_name " ++ en.pp)
              end;

 en.disambiguatedName =
   case q'' of
   disambiguated_package_name (_) ->
             disambiguated_error_name ([mkError (id.line, "Expression Name " ++ en.pp ++ " cannot have package name as component")]) |

   disambiguated_type_name (t) ->
             case t of
	      unknownTypeRep () ->
	        disambiguated_type_name (unknownTypeRep ()) |

	      errorTypeRep (errs) ->
	        disambiguated_error_name (errs) |

	      classTypeRep (ctr) ->
		if null (fieldSearchResult)
		      then (if hasUnknownSupers (t)
			    then disambiguated_error_name ( [ :: Error ])
			    else disambiguated_error_name ([mkError (id.line, "Class " ++ ctr.name ++ " does not have field " ++ id.lexeme) ]))

		--else if length (fieldSearchResult) > 1
	          --    then disambiguated_error_name ([mkError (id.line, "Class " ++ ctr.name ++ " has multiple fields " ++ id.lexeme) ])

		else if !firstField.is_static
                      then disambiguated_error_name ([mkError (id.line, "Static reference to non-static field " ++ firstField.name ++ " in " ++ en.pp)])

			-- check access

			-- check if final, if it's in a context that's expecting a variable (e.g. lhs) error

                else disambiguated_expr_name (disambiguated_static_field_access (t, id, firstField)) |

	      interfaceTypeRep (itr) ->
		if null (fieldSearchResult)
		      then (if hasUnknownSupers (t) -- extends unknown interfaces
			    then disambiguated_error_name ([ :: Error ])
			    else disambiguated_error_name ([mkError (id.line, "Interface " ++ itr.name ++ " does not have field " ++ id.lexeme) ]))

		--else if length (fieldSearchResult) > 1
	          --    then disambiguated_error_name ([mkError (id.line, "Interface " ++ itr.name ++ " has multiple fields " ++ id.lexeme) ])

			-- check access

			-- check if final, if it's in a context that's expecting a variable (e.g. lhs) error

	        else disambiguated_expr_name (disambiguated_static_field_access (t, id, firstField)) |

	      _ ->
	        disambiguated_error_name ([mkError (id.line, "Type that is not a class or an interface in ExprName " ++ en.pp)])
             end |

   disambiguated_expr_name (l) ->
            case l.typerep of 
		unknownTypeRep () ->
                  disambiguated_error_name ([ :: Error ]) |

		errorTypeRep (errs) ->
                  disambiguated_error_name (errs) |

	        classTypeRep (ctr) ->
	          if null (fieldSearchResult)
	             then (if hasUnknownSupers (l.typerep)
			   then disambiguated_error_name ([ :: Error ])
			   else disambiguated_error_name ([mkError (id.line, "Class " ++  ctr.name ++ " does not have field " ++ id.lexeme) ])
			   )
	          --else if length (fieldSearchResult) > 1
	            -- then disambiguated_error_name ([mkError (id.line, "Class " ++  ctr.name ++ " has multiple fields " ++ id.lexeme) ])

	      -- check access

	      -- check if final, requires a variable instead of value 

	          else disambiguated_expr_name (disambiguated_field_access (expr_lhs (l), id, firstField)) |

	        interfaceTypeRep (itr) ->
	          if null (fieldSearchResult)
	             then (if hasUnknownSupers (l.typerep) -- unknown superinterfaces
			   then disambiguated_error_name ([ :: Error ])
			   else disambiguated_error_name ([mkError (id.line, "Interface " ++  itr.name ++ " does not have field " ++ id.lexeme) ])
			   )
	          --else if length (fieldSearchResult) > 1
	             --then disambiguated_error_name ([mkError (id.line, "Interface " ++  itr.name ++ " has multiple fields " ++ id.lexeme) ])

	          -- check access
	      
	          -- check if the context requires a variable instead of value 

	          else disambiguated_expr_name (disambiguated_field_access (expr_lhs (l), id, firstField)) |

	        arrayTypeRep (_, _) ->
	          if id.lexeme == "length"
	          then disambiguated_expr_name (array_length_access (expr_lhs (l)))
	      
	          -- check if the context requires a variable instead of value 

	          else disambiguated_error_name ([mkError (id.line, "Only length can be accessed off arrays " ++ en.pp) ]) |

	         _ -> 
	          disambiguated_error_name ([mkError (id.line, "Type that is not a class, interface or array in ExprName " ++ en.pp)])
	      end |

   disambiguated_error_name (errs) ->
             disambiguated_error_name (errs) |

   _ ->
             error ("Internal compiler error 5 in production qualified_expr_name " ++ en.pp)
   end;
}

--------------------------------------------------
-- 6.5.7 Meaning of Method Names
--------------------------------------------------

attribute pp, basepp, disambiguatedName, env, type_env, enclosingType, line_no occurs on MethodName;

abstract production simple_method_name
mn::MethodName ::= id::Id_t  { 
 mn.pp = id.lexeme;
 mn.basepp = id.lexeme;
 mn.line_no = id.line;

 -- add stuff for instance vs static checking
 -- if final, forward to a "value_id" production

 local attribute methodSearchResult :: [ DclInfo ] ;
 methodSearchResult = lookupId (id, case mn.enclosingType of
                                        classTypeRep (ctr) -> ctr.methods |
                                        interfaceTypeRep (itr) -> itr.methods |
                                        _ -> error ("Internal compiler error in production simple_method_name " ++ mn.pp)
                                    end);

 mn.disambiguatedName = if null (methodSearchResult)
                              then (if hasUnknownSupers (mn.enclosingType)
			            then disambiguated_method_name ([unknown_method_dcl_rep (id.lexeme)])
				    else disambiguated_error_name ([mkError (id.line, "Unknown method " ++ id.lexeme)])
			           ) 
			else disambiguated_method_name (getMethodDclReps (methodSearchResult));
}

abstract production qualified_method_name
mn::MethodName ::= an::AmbiguousName id::Id_t {
 mn.pp = an.pp ++ "." ++ id.lexeme;
 mn.basepp = an.basepp ++ "." ++ id.lexeme;
 mn.line_no = id.line;

 -- Apply the rules from 6.5.7.2

 local attribute q :: DisambiguatedName;
 q = an.disambiguatedName;

 local attribute methodsToSearch :: [ ScopeEnv ];
 methodsToSearch = case q'' of
                   disambiguated_type_name (t) ->
                       case t of
                            classTypeRep (ctr) -> ctr.methods |
                            interfaceTypeRep (itr) -> itr.methods |
                            _ -> error ("Internal compiler error 1 in production qualified_method_name " ++ mn.pp)
		       end |

		   disambiguated_expr_name (l) ->
                       case l.typerep of
                            classTypeRep (ctr) -> ctr.methods |
                            interfaceTypeRep (itr) -> itr.methods |
                            _ -> error ("Internal compiler error 2 in production qualified_method_name " ++ mn.pp)
		       end |
		   _ ->
                       error ("Internal compiler error 3 in production qualified_method_name " ++ mn.pp)
                   end;

 local attribute methodSearchResult :: [ DclInfo ];
 methodSearchResult = lookupId (id, methodsToSearch);

 mn.disambiguatedName =
   case q'' of
   disambiguated_package_name (_) ->
             disambiguated_error_name ( [mkError (id.line, "Method Name " ++ mn.pp ++ " cannot have package name as component")]) |

   disambiguated_type_name (t) ->
             case t of
	      unknownTypeRep () -> 
	         disambiguated_method_name ([unknown_method_dcl_rep (id.lexeme)]) |

	      errorTypeRep (errs) -> 
	         disambiguated_error_name (errs) |

	      classTypeRep (ctr) ->
		 if null (methodSearchResult)
	              then (if hasUnknownSupers (t)
			    then disambiguated_method_name ([unknown_method_dcl_rep (id.lexeme)])
			    else disambiguated_error_name ( [mkError (id.line, "Class " ++ ctr.name ++ " does not have method " ++ id.lexeme) ])
			    )
		      else disambiguated_method_name (getMethodDclReps (methodSearchResult)) |
	      
	      interfaceTypeRep (itr) ->
		 if null (methodSearchResult)
	              then (if hasUnknownSupers (t) -- unknown superinterfaces
			    then disambiguated_method_name ([unknown_method_dcl_rep (id.lexeme)])
			    else disambiguated_error_name ([mkError (id.line, "Interface " ++ itr.name ++ " does not have method " ++ id.lexeme) ])
			    )
		 else disambiguated_method_name (getMethodDclReps (methodSearchResult)) |
	      
	      _ ->
	         disambiguated_error_name ([mkError (id.line, "Type that is not a class or an interface in MethodName " ++ mn.pp)]) 
              end |

   disambiguated_expr_name (l) ->
             case l.typerep of
	      unknownTypeRep () -> 
	         disambiguated_method_name ([unknown_method_dcl_rep (id.lexeme)]) |

	      errorTypeRep (errs) -> 
	         disambiguated_error_name (errs) |

	      classTypeRep (ctr) ->
		 if null (methodSearchResult)
	              then (if hasUnknownSupers (l.typerep)
			    then disambiguated_method_name ([unknown_method_dcl_rep (id.lexeme)])

			    else disambiguated_error_name ([mkError (id.line, "Class " ++ ctr.name ++ " does not have method " ++ id.lexeme) ])
			    )
		      else disambiguated_method_name (getMethodDclReps (methodSearchResult)) |
	      
	      interfaceTypeRep (itr) ->
		 if null (methodSearchResult)
	              then (if hasUnknownSupers (l.typerep) -- unknown superinterfaces
			    then disambiguated_method_name ([unknown_method_dcl_rep (id.lexeme)])

			    else disambiguated_error_name ([mkError (id.line, "Interface " ++ itr.name ++ " does not have method " ++ id.lexeme) ])
			    )
		 else disambiguated_method_name (getMethodDclReps (methodSearchResult)) |
	      
	      _ ->
	         disambiguated_error_name ( [mkError (id.line, "Type that is not a class or an interface in MethodName " ++ mn.pp)]) 
             end |

   disambiguated_error_name (errs) ->
            disambiguated_error_name (errs) |

   _ ->
            error ("Internal compiler error 5 in production qualified_method_name " ++ mn.pp)
  end;
}

--------------------------------------------------
-- 6.5.2 Meaning of Ambiguous Names
--------------------------------------------------

attribute pp, basepp, disambiguatedName, env, type_env, enclosingType occurs on AmbiguousName;

abstract production simple_ambiguous_name
an::AmbiguousName ::= id::Id_t {
 -- 6.5.2
 production attribute resolvedType :: TypeSearchResult;
 production attribute resolvedPackageOrType :: PackageOrTypeSearchResult;

 an.pp = id.lexeme;
 an.basepp = id.lexeme;

 -- add stuff for instance vs static checking
 -- if final, forward to a "value_id" production

 local attribute idSearchResult :: [ DclInfo ] ;
 idSearchResult = lookupIdOneScope (id, an.env ++ an.enclosingType.classtyperep.fields);

 local attribute firstId :: DclRep ;
 firstId = (head (idSearchResult)).dclrep ;

 an.disambiguatedName = if length (idSearchResult) > 1
                              then disambiguated_error_name ([mkError (id.line, "Multiple definitions for identifier " ++ id.lexeme ++ " in scope")])
                        else if length (idSearchResult) == 1
			      then (if firstId.is_local
				       then disambiguated_expr_name (bound_id (id, firstId))
				    else if firstId.is_param
				       then disambiguated_expr_name (bound_id (id, firstId))
				    else if firstId.is_field
				       then (if firstId.field_rep.is_static
					     then disambiguated_expr_name (disambiguated_static_field_access (an.enclosingType, id, firstId.field_rep))
					     else disambiguated_expr_name (simple_field_access (id, firstId.field_rep))
					    )
				    else error ("Internal compiler error in production simple_ambiguous_name: " ++ an.pp)
                                    )

			else case an.resolvedPackageOrTypeName of
				fully_qualified_name_none () -> disambiguated_error_name (resolvedPackageOrType.errors) |
				fully_qualified_name_unknown () -> disambiguated_error_name (resolvedPackageOrType.errors) |
				fqn -> if an.isPackage
					then disambiguated_package_name ( simple_package_name (id))
					else disambiguated_type_name (retrieveTypeRep2 (fqn.qualifiedName, an.type_env, an.file_name, id.line))
			     end;
}

abstract production qualified_ambiguous_name
andi::AmbiguousName ::= an::AmbiguousName id::Id_t {
 production attribute resolvedType :: TypeSearchResult;
 production attribute resolvedPackageOrType :: PackageOrTypeSearchResult;

 andi.pp = an.pp ++ "." ++ id.lexeme;
 andi.basepp = an.basepp ++ "." ++ id.lexeme;

 -- Apply the rules from 6.5.2

 local attribute q :: DisambiguatedName;
 q = an.disambiguatedName;

 local attribute fieldsToSearch :: [ ScopeEnv ];
 fieldsToSearch = case q'' of
                  disambiguated_type_name (t) ->
                         case t of
                            classTypeRep (ctr) -> ctr.fields |
                            interfaceTypeRep (itr) -> itr.fields |
                            _ -> error ("Internal compiler error 1 production qualified_ambiguous_name" ++ andi.pp)
		         end |
		  disambiguated_expr_name (l) ->
                         case l.typerep of
                            classTypeRep (ctr) -> ctr.fields |
                            interfaceTypeRep (itr) -> itr.fields |
                            _ -> error ("Internal compiler error 2 production qualified_ambiguous_name" ++ andi.pp)
		         end |
                  _ ->
                         error ("Internal compiler error 3 production qualified_ambiguous_name" ++ andi.pp)
                  end;

 local attribute fieldSearchResult :: [ DclInfo ];
 fieldSearchResult = lookupId (id, fieldsToSearch);

 local attribute firstField :: FieldDclRep;
 firstField = case (head (fieldSearchResult)).dclrep of
                  dcl_rep_field (fdr) -> fdr'' |
                  _ -> error ("Internal compiler error 4 production qualified_ambiguous_name" ++ andi.pp)
              end;

 local attribute typesToSearch :: [ ScopeEnv ];
 typesToSearch = case q'' of
                 disambiguated_type_name (t) ->
                         case t of
                            classTypeRep (ctr) -> ctr.innerTypes |
                            interfaceTypeRep (itr) -> itr.innerTypes |
                            _ -> error ("Internal compiler error 5 production qualified_ambiguous_name" ++ andi.pp)
		         end |

-- This is present in 6.5.2, but doesn't make sense, it's not present in qualified_expr_name, and javac gives an error if you
-- try to access an inner type off of an expression
--		  disambiguated_expr_name (l) ->
--                     then case l.typerep of
--                            classTypeRep (ctr) -> ctr.innerTypes |
--                            interfaceTypeRep (itr) -> itr.innerTypes |
--                            _ -> error ("Internal compiler error 6 production qualified_ambiguous_name" ++ andi.pp)
--		          end |
                  _ ->
                       error ("Internal compiler error 7 production qualified_ambiguous_name" ++ andi.pp)
                  end;

 local attribute typeSearchResult :: [ DclInfo ];
 typeSearchResult = lookupId (id, typesToSearch);

 local attribute firstType :: TypeRep;
 firstType = case (head (typeSearchResult)).dclrep of
                  dcl_rep_class (class_dcl_rep (_, t)) -> t'' |
                  dcl_rep_interface (interface_dcl_rep (_, t)) -> t'' |
                  _ -> error ("Internal compiler error 8 production qualified_ambiguous_name" ++ andi.pp)
              end;

 andi.disambiguatedName =
 case q'' of
 disambiguated_package_name (p) ->
	     case andi.resolvedPackageOrTypeName of
		fully_qualified_name_unknown () -> 
                    disambiguated_error_name ([mkError (id.line, "Unknown member " ++ id.lexeme ++ " in package " ++ an.pp) ]) |
		fully_qualified_name_none () -> 
                    disambiguated_error_name ([mkError (id.line, "Unknown member " ++ id.lexeme ++ " in package " ++ an.pp) ]) |
		fqn -> if andi.isPackage
			then disambiguated_package_name (qualified_package_name (p, id))
			else disambiguated_type_name (retrieveTypeRep2 (fqn.qualifiedName, andi.type_env, andi.file_name, id.line))
	     end |

 disambiguated_type_name (t) ->
             case t of
	      unknownTypeRep () ->
	        disambiguated_error_name ([ :: Error ]) |

	      errorTypeRep (errs) ->
	        disambiguated_error_name (errs) |

	      classTypeRep (ctr) ->
		if !null (fieldSearchResult)
		then (--if length (fieldSearchResult) > 1
		        --then disambiguated_error_name ([mkError (id.line, "Class " ++ ctr.name ++ " has multiple fields " ++ id.lexeme) ])

		      --else 
			if !firstField.is_static
			     then disambiguated_error_name ([mkError (id.line, "Static reference to non-static field " ++ firstField.name ++ " in " ++ andi.pp)])

			-- check access

			-- check if final, if it's in a context that's expecting a variable (e.g. lhs) error

		       else disambiguated_expr_name (disambiguated_static_field_access (t, id, firstField )))

		else if !null (typeSearchResult)
		  then (if length (typeSearchResult) > 1
		        then disambiguated_error_name ([mkError (id.line, "Class " ++ ctr.name ++ " has multiple member types " ++ id.lexeme) ])

			-- check access

		        else disambiguated_type_name (firstType))

		else if hasUnknownSupers (t)
			 then disambiguated_error_name ( [ :: Error ])

		else disambiguated_error_name ([mkError (id.line, "Class " ++ ctr.name ++ " does not have member " ++ id.lexeme) ]) |

	      interfaceTypeRep (itr) ->
		if !null (fieldSearchResult)
		then (--if length (fieldSearchResult) > 1
		       --then disambiguated_error_name ([mkError (id.line, "Interface " ++ itr.name ++ " has multiple fields " ++ id.lexeme) ])

			-- check access

			-- check if final, if it's in a context that's expecting a variable (e.g. lhs) error

		       --else 
			disambiguated_expr_name (disambiguated_static_field_access (t, id, firstField )))

		else if !null (typeSearchResult)
		  then (if length (typeSearchResult) > 1
		        then disambiguated_error_name ([mkError (id.line, "Interface " ++ itr.name ++ " has multiple member types " ++ id.lexeme) ])

			-- check access

		        else disambiguated_type_name (firstType))

		else if hasUnknownSupers (t)
			 then disambiguated_error_name ( [ :: Error ])

		else disambiguated_error_name ([mkError (id.line, "Interface " ++ itr.name ++ " does not have member " ++ id.lexeme) ]) |

	       _ -> 
		    disambiguated_error_name ([mkError (id.line, "Type that is not a class or interface in AmbiguousName " ++ andi.pp)])
	    end |

 disambiguated_expr_name (l) ->
           case l.typerep of 
		unknownTypeRep () ->
                  disambiguated_error_name ([ :: Error ]) |

		errorTypeRep (errs) ->
                  disambiguated_error_name (errs) |

	        classTypeRep (ctr) ->
	          if null (fieldSearchResult)
	             then (if hasUnknownSupers (l.typerep)
			   then disambiguated_error_name ([ :: Error ])
			   else disambiguated_error_name ([mkError (id.line, "Class " ++  ctr.name ++ " does not have field " ++ id.lexeme) ])
			   )
	          --else if length (fieldSearchResult) > 1
	            -- then disambiguated_error_name ([mkError (id.line, "Class " ++  ctr.name ++ " has multiple fields " ++ id.lexeme) ])

	      -- check access

	      -- check if final, requires a variable instead of value 

	          else disambiguated_expr_name (disambiguated_field_access (expr_lhs (l), id, firstField )) |

	        interfaceTypeRep (itr) ->
	          if null (fieldSearchResult)
	             then (if hasUnknownSupers (l.typerep) -- unknown superinterfaces
			   then disambiguated_error_name ([ :: Error ])
			   else disambiguated_error_name ([mkError (id.line, "Interface " ++  itr.name ++ " does not have field " ++ id.lexeme) ])
			   )
	          --else if length (fieldSearchResult) > 1
	            -- then disambiguated_error_name ([mkError (id.line, "Interface " ++  itr.name ++ " has multiple fields " ++ id.lexeme) ])

	          -- check access
	      
	          -- check if the context requires a variable instead of value 

	          else disambiguated_expr_name (disambiguated_field_access (expr_lhs (l), id, firstField )) |

	        arrayTypeRep (_, _) ->
	          if id.lexeme == "length"
	          then  disambiguated_expr_name (array_length_access (expr_lhs (l)))
	      
	          -- check if the context requires a variable instead of value 

	          else disambiguated_error_name ([mkError (id.line, "Only length can be accessed off arrays " ++ andi.pp) ]) |

	         _ -> 
	          disambiguated_error_name ([mkError (id.line, "Type that is not a class, interface or array in ExprName " ++ andi.pp)]) 
                end |

 disambiguated_error_name (errs) ->
        disambiguated_error_name (errs) |

 _ ->
        error ("Internal compiler error 9 in production qualified_ambiguous_name " ++ andi.pp)
 end;
}

function hasUnknownSupers
Boolean ::= t::TypeRep {
 return case t'' of
        classTypeRep (ct) -> ct.name != "Object" && (equality_check (ct.superClass, unknownTypeRep ()) || hasUnknownSupers (ct.superClass)) |
        interfaceTypeRep (it) -> false |
        _ -> error ("Internal compiler error in function hasUnknownSupers " ++ t.pp)
        end;
}

function getMethodDclReps
[ MethodDclRep ] ::= dis::[ DclInfo ] {
 return if null (dis)
	then []
	else (case head (dis) of
		dclInfo (_, dcl_rep_method (mdr), _) -> [ mdr'' ] |
		_ -> error ("Internal compiler error in getMethodDclReps")
	      end) ++ getMethodDclReps (tail (dis));
}




function mk_ambig_name
AmbiguousName ::= ns::[String]
{
 return mk_ambig_name_helper( string_list_reverse( ns ) ) ;
}

function string_list_reverse
[String] ::= ss::[String]
{ 
 return if   null(ss) 
        then [ ]
        else string_list_reverse(tail(ss)) ++ [ head(ss) ] ;
}

function mk_ambig_name_helper
AmbiguousName ::= ns::[String]
{
 return if   null(ns)
        then error("Internal error: illegal use of mk_ambig_name.")
        else 
        if   length(ns) == 1
        then simple_ambiguous_name ( terminal(Id_t, head(ns) ) )
        else qualified_ambiguous_name ( mk_ambig_name_helper ( tail(ns) ) ,
                                        terminal(Id_t, head(ns) ) ) ;
}
