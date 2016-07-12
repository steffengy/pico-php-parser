use pest::prelude::*;
use std::borrow::Cow;
use std::collections::LinkedList;
use std::num::ParseIntError;
use std::string::FromUtf8Error;
use ast::{Expr, CatchClause, ClassDecl, ClassMember, ClassModifier, Decl, FunctionDecl, Modifiers, UnaryOp,
    Op, ParsedItem, ParamDefinition, Path, Ty, IncludeTy, TraitUse, UseClause, Visibility};

#[derive(Debug)]
pub enum ParseError {
    Utf8(FromUtf8Error),
    ParseInt(ParseIntError),
}

impl From<FromUtf8Error> for ParseError {
    fn from(err: FromUtf8Error) -> ParseError {
        ParseError::Utf8(err)
    }
}

impl From<ParseIntError> for ParseError {
    fn from(err: ParseIntError) -> ParseError {
        ParseError::ParseInt(err)
    }
}

#[derive(Debug)]
pub enum IdxExpr<'a> {
    ArrayIdx(Expr<'a>),
    Call(Vec<Expr<'a>>),
    ObjMember(Expr<'a>),
    StaticMember(Expr<'a>),
}

/// (mostly) based on: https://github.com/php/php-langspec/commit/46a218a09ef6156e419194122a440e5400967fe2
impl_rdp! {
    grammar! {
        // Simple tokens from various sections (basic)
        digit                           =  { digit_silent }
        digit_silent                    = _{ ['0'..'9'] }
        nonzero_digit                   =  { nonzero_digit_silent }
        nonzero_digit_silent            = _{ ['1'..'9'] }
        octal_digit                     =  { octal_digit_silent }
        octal_digit_silent              = _{ ['0'..'7'] }
        binary_digit                    =  { ["0"] | ["1"] }
        hexadecimal_digit               =  { hexadecimal_digit_silent }
        hexadecimal_digit_silent        = _{ ['0'..'9'] | ['a'..'f'] | ['A'..'F'] }
        nondigit                        =  { nondigit_silent }
        nondigit_silent                 = _{ ["_"] | ['a'..'z'] | ['A'..'Z'] }
        sign                            =  { ["+"] | ["-"] }
        new_line                        =  { (["\r"] ~ ["\n"]) | ["\r"] | ["\n"] }

        // Section: Program Structure
        script                          =  { soi ~ script_section+ ~ eoi }
        start_tag                       = _{ ["<?php"] | ["<?="] }
        end_tag                         = _{ ["?>"] }
        script_section                  =  { text? ~ start_tag ~ statement_list? ~ (end_tag ~ text?)? }
        text                            =  { (!start_tag ~ any)+ }

        // Section: Names
        variable_name                   =  { ["$"] ~ name }
        name                            = @{ nondigit_silent ~ (digit_silent | nondigit_silent)* } //TODO: U+0080–U+00ff (name_nondigit)

        //literal translation from reference: (does not work)
        //namespace_name                = @{ name | (namespace_name ~ ["\\"] ~ name) }
        //namespace_name_as_a_prefix    = @{ (["\\"]? ~ namespace_name ~ ["\\"]) | (["namespace"] ~ ["\\"] ~ (namespace_name ~ ["\\"])?) | ["\\"] }

        // This name_item allows parsing without consuming adjacent names (which do not belong to the container)
        // as in the case of e.g. qualified_name
        namespace_name_item             = @{ name }
        namespace_name                  = @{ namespace_name_item ~ (["\\"] ~ namespace_name_item)* }
        namespace_name_as_a_prefix      = @{
            (["\\"]? ~ (namespace_name_item ~ ["\\"])*) |
            (["namespace"] ~ ["\\"] ~ (namespace_name_item ~ ["\\"])*)
        }
        qualified_name                  = @{ namespace_name_as_a_prefix? ~ name }

        // Section: Literals
        literal                         = @{ integer_literal | floating_literal | string_literal }
        //        : Integer Literals
        integer_literal                 = @{ decimal_literal | octal_literal | hexacdecimal_literal | binary_literal }
        decimal_literal                 = @{ nonzero_digit_silent ~ digit_silent* }
        octal_literal                   = @{ ["0"] ~ octal_digit_sequence }
        octal_digit_sequence            = @{ octal_digit_silent* }
        hexacdecimal_prefix             = _{ ["0x"] | ["0X"] }
        hexacdecimal_literal            = @{ hexacdecimal_prefix ~ hexadecimal_digit* }
        binary_prefix                   = _{ ["0b"] | ["0B"] }
        binary_literal                  = @{ binary_prefix ~ binary_digit* }
        //        : Floating Literals
        floating_literal                = @{ (fractional_literal ~ exponent_part?) | (digit+ ~ exponent_part) }
        fractional_literal              = @{ (digit* ~ ["."] ~ digit+) | (digit+ ~ ["."]) }
        exponent_part                   = @{ (["e"] | ["E"]) ~ sign? ~ digit+ }
        //        : String Literals (incomplete)
        string_literal                  = @{ single_quoted_string_literal | double_quoted_string_literal | heredoc_string_literal | nowdoc_string_literal }
        single_quoted_string_literal    = @{ ["b"]? ~ ["'"] ~ sq_char* ~ ["'"] }
        sq_char                         = @{ sq_escape_sequence | (!(["'"] | ["\\"]) ~ any) }
        sq_escape_sequence              = @{ ["\\'"] | ["\\\\"] | ["\\"] }
        double_quoted_string_literal    = @{ ["b"]? ~ ["\""] ~ dq_char* ~ ["\""] }
        dq_char                         = @{ dq_escape_sequence | (!(["\""] | ["\\"]) ~ any) } // todo, incomplete, as below
        dq_escape_sequence              = @{ dq_simple_escape_sequence | dq_octal_escape_sequence | dq_hexadecimal_escape_sequence | dq_unicode_escape_sequence }
        dq_simple_escape_sequence       = @{ ["\\\""] | ["\\\\"] | ["\\$"] | ["\\e"] | ["\\f"] | ["\\n"] | ["\\r"] | ["\\t"] | ["\\v"] }
        dq_octal_escape_sequence        = @{ ["\\"] ~ octal_sequence }
        octal_sequence                  = @{
            (octal_digit_silent ~ octal_digit_silent ~ octal_digit_silent) |
            (octal_digit_silent ~ octal_digit_silent) |
            octal_digit_silent
        }

        dq_hexadecimal_escape_sequence  = @{ (["\\x"] | ["\\X"]) ~ hexadecimal_sequence }
        hexadecimal_sequence            =  { hexadecimal_digit_silent ~ hexadecimal_digit_silent? }
        dq_unicode_escape_sequence      = @{ ["\\u{"] ~ hexadecimal_digit+ ~ ["}"] }
        string_variable                 = @{ (variable_name ~ offset_or_property) | (["$"] ~ ["{"] ~ expression ~ ["}"]) }
        offset_or_property              = @{ offset_in_string | property_in_string }
        offset_in_string                = @{ ["["] ~ (name | variable_name ~ integer_literal) ~ ["]"] }
        property_in_string              = @{ ["->"] ~ name }

        // features nobody uses...
        heredoc_string_literal          = @{ ["<<<"] ~ hd_start_identifier ~ new_line ~ hd_char* ~ new_line ~ hd_end_identifier ~ [";"]? ~ new_line }
        hd_start_identifier             = @{ name | (["\""] ~ name ~ ["\""]) }
        hd_end_identifier               = @{ name }
        hd_char                         = @{ hd_escape_sequence | (!(["\\"]) ~ any) } // todo: incomplete, as above
        hd_escape_sequence              = @{ hd_simple_escape_sequence | dq_octal_escape_sequence | dq_hexadecimal_escape_sequence | dq_unicode_escape_sequence }
        hd_simple_escape_sequence       =  { ["\\\\"] | ["\\$"] | ["\\e"] | ["\\f"] | ["\\n"] | ["\\r"] | ["\\t"] | ["\\v"] }
        nowdoc_string_literal           = @{ ["<<<"] ~ ["'"] ~ name ~ ["'"] ~ new_line ~ hd_char* ~ new_line ~ name ~ [";"]? ~ new_line }

        // Section: Variables
        function_static_declaration     =  { ["static"] ~ static_variable_name_list ~ [";"] }
        static_variable_name_list       =  { static_variable_declaration }
        static_variable_declaration     =  { variable_name ~ function_static_initializer? }
        function_static_initializer     =  { ["="] ~ constant_expression }
        global_declaration              =  { ["global"] ~ variable_name_list ~ [";"] }
        variable_name_list              =  { expression | (variable_name_list ~ [","] ~ expression) }

        // Section: Expressions
        // In the reference this contains "constant_expression", we use "array_creation_expression" to prevent infinite recursion
        primary_expression              =  {
            intrinsic | variable_name | constant | anonym_func_creation_expression | qualified_name | literal |
            array_creation_expression |
            ["$this"]
        }
        // added to parse constants explicitly, not in reference
        constant                        =  { constant_true | constant_false | constant_null }
        constant_true                   =  { ["true"] }
        constant_false                  =  { ["false"] }
        constant_null                   =  { ["null"] }
        intrinsic                       =  { intrinsic_construct | intrinsic_operator }
        intrinsic_construct             = _{ echo_intrinsic | list_intrinsic | unset_intrinsic }
        intrinsic_operator              = _{ array_intrinsic | empty_intrinsic | eval_intrinsic | exit_intrinsic | isset_intrinsic | print_intrinsic }
        array_intrinsic                 =  { ["array"] ~ ["("] ~ array_initializer? ~ [")"] }
        echo_intrinsic                  =  { ["echo"] ~ expression_list_one_or_more }
        empty_intrinsic                 =  { ["empty"] ~ ["("] ~ expression ~ [")"] }
        eval_intrinsic                  =  { ["eval"] ~ ["("] ~ expression ~ [")"] }
        exit_or_die                     =  { ["exit"] | ["die"] }
        exit_intrinsic                  =  { (exit_or_die ~ expression?) | (exit_or_die ~ ["("] ~ expression ~ [")"]) }
        isset_intrinsic                 =  { ["isset"] ~ ["("] ~ expression_list_one_or_more ~ [")"] }
        expression_list_one_or_more     = _{ expression ~ ([","] ~ expression)* }
        list_intrinsic                  =  { ["list"] ~ ["("] ~ list_expression_list? ~ [")"] }
        list_expression_list            =  {
            (list_or_variable | keyed_list_expression | list_item_empty) ~ (
                [","] ~ (list_or_variable | keyed_list_expression | list_item_empty)
            )*
        }
        list_item_empty =  { [","] }
        //unkeyed_list_expression_list  =  list_or_variable
        keyed_list_expression           =  { expression ~ ["=>"] ~ list_or_variable }
        list_or_variable                =  { list_intrinsic | expression }
        print_intrinsic                 =  { (["print"] ~ expression) | (["print"] ~ ["("] ~ expression ~ [")"]) }
        unset_intrinsic                 =  { ["unset"] ~ ["("] ~ expression_list_one_or_more ~ [")"] }
        anonym_func_creation_expression =  {
            ["static"]? ~ ["function"] ~ function_ret_reference? ~ ["("] ~ parameter_declaration_list? ~ function_definition_param_end
            ~ return_type? ~ anonym_func_use_clause? ~ compound_statement
        }
        anonym_func_use_clause          = _{ ["use"] ~ ["("] ~ (use_variable_name_expr ~ ([","] ~ use_variable_name_expr)*) ~ [")"] }
        use_variable_name_expr          =  { use_variable_byref? ~ variable_name }
        use_variable_byref              =  { ["&"] }

        //Section: Postfix Operators
        postfix_expression_internal     = _{
            ref_expression | clone_expression | object_creation_expression | array_creation_expression | primary_expression
            /*TODO: | scope_resolution_expression */
        }
        // for PEST modified postfix_expression, which handles subscript and member selection
        postfix_expression              =  {
            postfix_expression_internal ~
            (subscript | function_call | member_selection | scope_resolution)* ~
            increment_or_decrement_op?
        }
        increment_or_decrement_op       =  { op_increment | op_decrement }
        ref_expression                  =  { ["&"] ~ assignment_expression }
        clone_expression                =  { ["clone"] ~ expression }
        object_creation_expression      =  {
            ( ["new"] ~ class_type_designator ~ ["("] ~ (!function_call_end ~ argument_expression_list)? ~ function_call_end ) |
            ( ["new"] ~ ["class"] ~ ["("] ~ argument_expression_list? ~ [")"] ~ class_base_clause? ~ class_interface_clause? ~ (["{"] ~ ["}"])? ) | //TODO: weird?
            ( ["new"] ~ class_base_clause? ~ class_interface_clause? ~ ["{"] ~ class_member_declarations? ~ ["}"] )
        }
        class_type_designator           = _{ qualified_name | expression }
        array_creation_expression       =  { (["array"] ~ ["("] ~ array_initializer? ~ [")"]) | (["["] ~ array_initializer? ~ ["]"]) }
        array_initializer               = _{ array_initializer_list ~ [","]? }
        array_initializer_list          = _{ array_element_initializer ~  ([","] ~ array_element_initializer)* }
        array_element_initializer       =  { (element_key ~ ["=>"] ~ element_value) | element_value }
        element_key                     =  { expression }
        element_value                   =  { expression }
        subscript                       =  { ["["] ~ expression? ~ subscript_end }
        subscript_end                   =  { ["]"] }
        function_call_expression        =  { qualified_name ~ function_call }
        function_call                   =  { ["("] ~ (![")"] ~ argument_expression_list)? ~ function_call_end }
        function_call_end               =  { [")"] }
        argument_expression_list        = _{ (argument_expression ~ ([","] ~ argument_expression)*) }
        argument_expression             =  { variadic_unpacking | assignment_expression }
        variadic_unpacking              =  { ["..."] ~ assignment_expression }
        member_selection                =  { ["->"] ~ member_selection_designator }
        //TODO: use var_name_creation_expression insteadof variable_name?
        member_selection_designator     = _{ curly_braced_expression | variable_name | name }
        curly_braced_expression         =  { ["{"] ~ expression ~ curly_braced_expression_end }
        curly_braced_expression_end     =  { ["}"] }
        op_increment                    =  { ["++"] }
        op_decrement                    =  { ["--"] }
        scope_resolution                =  { ["::"] ~ (member_selection_designator | scope_class) }
        scope_class                     =  { ["class"] }

        /*TODO:
        scope_resolution_qualifier      =  { relative_scope | qualified_name | expression }
        relative_scope                  =  { ["self"] | ["parent"] | ["static"] }
        */

        // Section: Unary Operators
        unary_expression_internal       = _{
            postfix_expression | error_control_expression |
            shell_command_expression | var_name_creation_expression
        }
        unary_expression                =  {
            unary_expression_internal ~ instanceof_expression?
        }
        unary_operator                  =  { op_add | op_sub | op_not | op_bitwise_not }
        error_control_expression        =  { ["@"] ~ expression }
        shell_command_expression        =  { ["`"] ~ dq_char* ~ ["`"] }
        cast_expression                 =  { ["("] ~ cast_type ~ [")"] ~ expression }
        cast_type                       =  {
            type_array | type_binary | type_bool | type_boolean | type_double | type_int | type_integer | type_float | type_object | type_real |
            type_string | type_unset
        }
        // TODO: this shouldn't be ["$"] ~ expression (expression is too much for this subrule)
        var_name_creation_expression    =  { (["$"] ~ expression) | (["$"] ~ ["{"] ~ expression ~ ["}"]) }

        // Section: instanceof Operator
        instanceof_expression           =  { ["instanceof"] ~ instanceof_type_designator }
        instanceof_type_designator      = _{ qualified_name | variable_name }

        // Unary/Binary Operators from various sections rewritten to use precedence-climbing
        _exponentiation                 = _{
            { cast_expression | (["("] ~ assignment_expression ~ [")"]) | unary_expression }
            exponentiation = {< op_pow }
        }
        _unary                          = _{ unary | _exponentiation }
        unary                           =  { (increment_or_decrement_op ~ _exponentiation) | (unary_operator ~ _unary) }
        binary_expression = {
            { _unary }
            logical_inc_or_expression_1 =  { op_logical_inc_or_1 }
            logical_and_expression      =  { op_and }
            bitwise_inc_or_expression   =  { op_bitwise_inc_or }
            bitwise_exc_or_expression   =  { op_bitwise_exc_or }
            bitwise_and_expression      =  { op_bitwise_and }
            equality_expression         =  { op_equality_identical | op_equality_not_identical | op_equality_eq | op_equality_neq | op_equality_uneq }
            relational_expression       =  { op_spaceship | op_le | op_ge | op_lt | op_gt }
            shift_expression            =  { op_shr | op_shl }
            additive_expression         =  { op_add | op_sub | op_concat }
            multiplicative_expression   =  { op_mul | op_div | op_mod }
        }
        op_not                          =  { ["!"] }
        op_bitwise_not                  =  { ["~"] }
        op_logical_inc_or_1             =  { ["||"] }
        op_and                          =  { ["&&"] }
        op_bitwise_inc_or               =  { ["|"] }
        op_bitwise_exc_or               =  { ["^"] }
        op_bitwise_and                  =  { ["&"] }
        op_equality_eq                  =  { ["=="] }
        op_equality_neq                 =  { ["!="] }
        op_equality_uneq                =  { ["<>"] }
        op_equality_identical           =  { ["==="] }
        op_equality_not_identical       =  { ["!=="] }
        op_lt                           =  { ["<"] }
        op_gt                           =  { [">"] }
        op_le                           =  { ["<="] }
        op_ge                           =  { [">="] }
        op_spaceship                    =  { ["<=>"] }
        op_shr                          =  { [">>"] }
        op_shl                          =  { ["<<"] }
        op_add                          =  { ["+"] }
        op_sub                          =  { ["-"] }
        op_concat                       =  { ["."] }
        op_mul                          =  { ["*"] }
        op_div                          =  { ["/"] }
        op_mod                          =  { ["%"] }
        op_pow                          =  { ["**"] }

        // Section: Conditional Operator
        conditional_expression          =  {
            ternary_expression |
            binary_expression
        }

        ternary_expression              =  {
            binary_expression ~ ["?"] ~ expression? ~ [":"] ~ conditional_expression
        }

        // Section: Coalesce Operator (TODO: in the documentation it's logical-inc-OR-expression, which doesn't exist. typo?)
        coalesce_expression            =  { binary_expression ~ ["??"] ~ expression }

        // Section: Assignment Operators
        assignment_expression          =  {
            (unary_operator* ~ (coalesce_expression | byref_assignment_expression | simple_assignment_expression | compound_assignment_expression | ternary_expression)) |
            binary_expression
        }
        simple_assignment_expression   =  { unary_expression ~ ["="] ~ assignment_expression }
        byref_assignment_expression    =  { unary_expression ~ ["="] ~ ["&"] ~ assignment_expression }
        compound_assignment_expression =  { unary_expression ~ compound_assignment_operator ~ assignment_expression }
        compound_assignment_operator   =  {
            compound_op_pow | compound_op_mul | compound_op_div | compound_op_mod | compound_op_add | compound_op_sub |
            compound_op_concat | compound_op_shl | compound_op_shr | compound_op_and | compound_op_xor | compound_op_or
        }
        compound_op_pow                =  { ["**="] }
        compound_op_mul                =  { ["*="] }
        compound_op_div                =  { ["/="] }
        compound_op_mod                =  { ["%="] }
        compound_op_add                =  { ["+="] }
        compound_op_sub                =  { ["-="] }
        compound_op_concat             =  { [".="] }
        compound_op_shl                =  { ["<<="] }
        compound_op_shr                =  { [">>="] }
        // compound bitwise-and
        compound_op_and                =  { ["&="] }
        compound_op_xor                =  { ["^="] }
        compound_op_or                 =  { ["|="] }

        // Section: Logical Operators (Form 2) - TODO: won't work, not ported yet, precedence wrong? (since after assignment?)
        /*logical_AND_expression_2        = {
            assignment_expression |
            (logical_AND_expression_2 ~ ["and"] ~ assignment_expression)
        }
        logical_exc_OR_expression       = {
            logical_AND_expression_2 |
            (logical_exc_OR_expression ~ ["xor"] ~ logical_AND_expression_2)
        }
        logical_inc_OR_expression_2     = {
            logical_exc_OR_expression |
            (logical_inc_OR_expression_2 ~ ["or"] ~ logical_exc_OR_expression)
        }*/

        // Section: yield-Operator
        // JUMP ahead to assignment_expression, since logical_inc_OR_expression_2 and the above ones are not supported yet
        // (will have to be in precedence climbing far above)
        // which may have the wrong precedence nevertheless (since there're expression types between)
        yield_expression                = {
            assignment_expression |
            (["yield"] ~ array_element_initializer)
        }

        // Section: Script Inclusion Operators
        expression                      = {
            include_once_expression | include_expression | require_once_expression | require_expression | yield_expression
        }
        expression_or_braced_expression = _{ (["("] ~ expression ~ [")"]) | expression }
        include_expression              =  { ["include"] ~ expression_or_braced_expression }
        include_once_expression         =  { ["include_once"] ~ expression_or_braced_expression }
        require_expression              =  { ["require"] ~ expression_or_braced_expression }
        require_once_expression         =  { ["require_once"] ~ expression_or_braced_expression }

        // Section: Constant-Expressions
        constant_expression             =  { array_creation_expression | expression }

        // Statements
        // Section: General
        statement                       =  {
            compound_statement | named_label_statement | selection_statement | iteration_statement |
            jump_statement | expression_statement | try_statement | declare_statement | const_declaration | function_definition | class_declaration |
            interface_declaration | trait_declaration | namespace_definition | namespace_use_declaration |
            global_declaration | function_static_declaration
        }

        // Section: Compound Statement
        compound_statement              =  { ["{"] ~ (!["}"] ~ statement_list)? ~ compound_statement_end }
        compound_statement_end          =  { ["}"] }
        statement_list                  = _{ statement+ }

        // Section: Labeled Statement
        named_label_statement           =  { !(["default"] ~ case_label_terminator) ~ name ~ [":"] ~ statement }
        case_statement                  =  { (["default"] ~ case_label_terminator) | (["case"] ~ expression ~ case_label_terminator) }
        case_label_terminator           = _{ [":"] | [";"] }

        // Section: Expression Statements
        expression_statement            =  { expression? ~ [";"] }
        selection_statement             =  { if_statement | switch_statement }
        if_statement                    =  {
            (["if"] ~ ["("] ~ expression ~ [")"]) ~ (
                (statement ~ else_clause_1?) |
                ([":"] ~ statement_list ~ else_clause_2? ~ ["endif"] ~ [";"])
            )
        }
        else_clause_1                   =  { ["else"] ~ statement }
        else_clause_2                   =  { ["else"] ~ [":"] ~ statement_list }
        switch_statement                =  {
            ( ["switch"] ~ ["("] ~ expression ~ [")"] ) ~ (
                ( ["{"] ~ (case_statement+ ~ statement_list?)* ~ ["}"] ) |
                ( [":"] ~ (case_statement+ ~ statement_list?)* ~ ["endswitch"] ~ [";"] )
            )
        }

        // Section: Iteration Statements
        iteration_statement             =  { while_statement | do_statement | for_statement | foreach_statement }
        while_statement                 =  {
            ( ["while"] ~ ["("] ~ expression ~ [")"] ) ~ (
                statement |
                ([":"] ~ statement_list ~ ["endwhile"] ~ [";"])
            )
        }
        do_statement                    =  { ["do"] ~ statement ~ ["while"] ~ ["("] ~ expression ~ [")"] ~ [";"] }
        for_statement                   =  {
            ( ["for"] ~ ["("] ~ for_initializer? ~ [","] ~ for_control? ~ [","] ~ for_end_of_loop? ~ [")"] ) ~ (
                statement |
                ([":"] ~ statement_list ~ ["endfor"] ~ [";"])
            )
        }
        for_initializer                 =  { for_expression_group }
        for_control                     =  { for_expression_group }
        for_end_of_loop                 =  { for_expression_group }
        for_expression_group            =  { expression | (for_expression_group ~ [","] ~ expression) }
        foreach_statement               =  {
            ( ["foreach"] ~ ["("] ~ foreach_collection_name ~ ["as"] ~ foreach_key? ~ foreach_value ~ [")"] ) ~ (
                statement |
                ([":"] ~ statement_list ~ ["endforeach"] ~ [";"])
            )
        }
        foreach_collection_name         = _{ expression }
        foreach_key                     =  { expression ~ ["=>"] }
        foreach_value                   =  { list_intrinsic | expression }

        // Section: Jump Statements
        jump_statement                  =  { goto_statement | continue_statement | break_statement | return_statement | throw_statement }
        goto_statement                  =  { ["goto"] ~ name ~ [";"] }
        continue_statement              =  { ["continue"] ~ breakout_level? ~ [";"] }
        breakout_level                  =  { integer_literal }
        break_statement                 =  { ["break"] ~ breakout_level? ~ [";"] }
        return_statement                =  { ["return"] ~ expression? ~ [";"] }
        throw_statement                 =  { ["throw"] ~ expression ~ [";"] }

        // Section: try Statement
        try_statement                   =  { (["try"] ~ compound_statement) ~ (catch_clause* ~ finally_clause?) }
        catch_clause                    =  { ["catch"] ~ ["("] ~ qualified_name ~ variable_name ~ [")"] ~ compound_statement }
        finally_clause                  =  { ["finally"] ~ compound_statement }

        // Section: declare Statement
        declare_statement               = {
            ( ["declare"] ~ ["("] ~ declare_directive ~ [")"] ) ~ (
                statement |
                ([":"] ~ statement_list ~ ["enddeclare"] ~ [";"]) |
                [";"]
            )
        }
        declare_directive               = { (["ticks"] | ["encoding"] | ["strict_types"]) ~ ["="] ~ literal }

        // Section: Functions
        function_definition             = _{ function_definition_header ~ compound_statement }
        function_ret_reference          =  { ["&"] }
        function_definition_header      =  { ["function"] ~ function_ret_reference? ~ name ~ ["("] ~ parameter_declaration_list? ~ function_definition_param_end ~ return_type? }
        function_definition_param_end   =  { [")"] }
        parameter_expression            = _{ parameter_declaration | variadic_parameter }
        parameter_declaration_list      = _{ parameter_expression ~ ([","] ~ parameter_expression)* }
        parameter_declaration           =  { type_declaration? ~ parameter_as_ref? ~ variable_name ~ default_argument_specifier? }
        parameter_as_ref                =  { ["&"] }
        variadic_parameter              =  { type_declaration? ~ parameter_as_ref? ~ ["..."] ~ variable_name }
        return_type                     =  { ([":"] ~ type_declaration) | ([":"] ~ ["void"]) }
        type_array                      =  { ["array"] }
        type_callable                   =  { ["callable"] }
        type_declaration                =  { type_declaration_ty | qualified_name }
        type_declaration_ty             =  { type_array | type_callable | scalar_type }
        type_bool                       =  { ["bool"] }
        type_boolean                    =  { ["boolean"] }
        type_binary                     =  { ["binary"] }
        type_double                     =  { ["double"] }
        type_float                      =  { ["float"] }
        type_int                        =  { ["int"] }
        type_integer                    =  { ["integer"] }
        type_string                     =  { ["string"] }
        type_object                     =  { ["object"] }
        type_unset                      =  { ["unset"] }
        type_real                       =  { ["real"] }
        scalar_type                     = _{ type_bool | type_float | type_int | type_string }
        default_argument_specifier      =  { ["="] ~ constant_expression }

        // Section: Classes
        class_declaration               =  { class_modifier? ~ ["class"] ~ name ~ class_base_clause? ~ class_interface_clause? ~ ["{"] ~ (!["}"] ~ class_member_declarations)? ~ ["}"] }
        class_modifier                  =  { class_modifier_abstract | class_modifier_final }
        class_modifier_abstract         =  { ["abstract"] }
        class_modifier_final            =  { ["final"] }
        class_base_clause               =  { ["extends"] ~ qualified_name }
        class_interface_clause          =  { ["implements"] ~ qualified_name ~ ([","] ~ qualified_name)* }
        class_member_declarations       = _{ class_member_declaration* }
        class_member_declaration        =  { const_declaration | property_declaration | method_declaration | constructor_declaration | destructor_declaration | trait_use_clause }
        const_declaration               =  { ["const"] ~ name ~ ["="] ~ constant_expression ~ [";"] }
        property_declaration            =  { property_modifier ~ variable_name ~ property_initializer? ~ [";"] }
        property_modifier               = _{ ["var"] | (visibility_modifier ~ static_modifier?) | (static_modifier ~ visibility_modifier?) }
        visibility_modifier             =  { visibility_public | visibility_protected | visibility_private }
        visibility_public               =  { ["public"] }
        visibility_protected            =  { ["protected"] }
        visibility_private              =  { ["private"] }
        static_modifier                 =  { ["static"] }
        property_initializer            =  { ["="] ~ constant_expression }
        method_declaration              =  { (method_modifiers? ~ function_definition) | (method_modifiers ~ function_definition_header ~ [";"]) }
        method_modifiers                = _{ method_modifier+ }
        method_modifier                 = _{ visibility_modifier | static_modifier | class_modifier }
        constructor_declaration         =  { method_modifiers ~ ["function"] ~ function_ret_reference? ~ ["__construct"] ~ ["("] ~ parameter_declaration_list? ~ [")"] ~ compound_statement }
        destructor_declaration          =  { method_modifiers ~ ["function"] ~ function_ret_reference? ~ ["__destruct"] ~ ["("] ~ [")"] ~ compound_statement }

        // Section: Interfaces
        interface_declaration           =  { ["interface"] ~ name ~ interface_base_clause? ~ ["{"] ~ interface_member_declaration* ~ ["}"] }
        interface_base_clause           =  { ["extends"] ~ qualified_name ~ ([","] ~ qualified_name)* }
        interface_member_declaration    =  { const_declaration | method_declaration }

        // Section: Traits
        trait_declaration               =  { ["trait"] ~ name ~ ["{"] ~ trait_member_declaration* ~ ["}"] }
        trait_member_declaration        =  { property_declaration | method_declaration | constructor_declaration | destructor_declaration | trait_use_clause }
        trait_use_clause                =  { ["use"] ~ trait_name_list ~ trait_use_specification }
        trait_name_list                 =  { qualified_name ~ ([","] ~ qualified_name)* }
        trait_use_specification         =  { [";"] | (["{"] ~ trait_select_and_alias_clause* ~ ["}"]) }
        trait_select_and_alias_clause   =  { (trait_select_insteadof_clause | trait_alias_as_clause) ~ [";"] }
        trait_select_insteadof_clause   =  { name ~ ["insteadof"] ~ name }
        trait_alias_as_clause           =  {
            name ~ ["as"] ~ (
                (visibility_modifier? ~ name) |
                (visibility_modifier ~ name?)
            )
        }

        // Section: Namespaces
        namespace_definition            =  {
            ["namespace"] ~ (
                (namespace_name ~ [";"]) |
                (namespace_name? ~ compound_statement)
            )
        }
        namespace_use_declaration       =  {
            ["use"] ~ (
                (namespace_function_or_const? ~ namespace_use_clauses) |
                (namespace_function_or_const ~ ["\\"]? ~ namespace_name ~ ["\\"] ~ ["{"] ~ namespace_use_group_clauses_1 ~ ["}"]) |
                (["\\"]? ~ namespace_name ~ ["\\"] ~ ["{"] ~ namespace_use_group_clauses_2 ~ ["}"])
            ) ~ [";"]
        }
        namespace_use_clauses           = _{ namespace_use_clause ~ ([","] ~ namespace_use_clause)* }
        namespace_use_clause            =  { qualified_name ~ namespace_aliasing_clause? }
        namespace_aliasing_clause       =  { ["as"] ~ name }
        namespace_function_or_const     =  { ["function"] | ["const"] }
        namespace_use_group_clauses_1   =  { namespace_use_group_clause_1 ~ ([","] ~ namespace_use_group_clause_1)* }
        namespace_use_group_clause_1    =  { namespace_name ~ namespace_aliasing_clause? }
        namespace_use_group_clauses_2   =  { namespace_use_group_clause_2 ~ ([","] ~ namespace_use_group_clause_2)* }
        namespace_use_group_clause_2    =  { namespace_function_or_const? ~ namespace_name ~ namespace_aliasing_clause? }

        comment = _{
            ( (["//"] | ["#"]) ~ (!(["\r"] | ["\n"]) ~ any)* ~ (["\n"] | ["\r\n"] | ["\r"] | eoi) ) |
            (["/*"] ~ (!(["*/"]) ~ any)* ~ ["*/"])
        }

        whitespace = _{ [" "] | ["\t"] | ["\u{000C}"] | ["\r"] | ["\n"] }
    }

    process! {
        main(&self) -> Result<Vec<ParsedItem<'input>>, ParseError> {
            (_: script, pfile: _script_sections()) => Ok(try!(pfile).into_iter().collect())
        }

        _script_sections(&self) -> Result<LinkedList<ParsedItem<'input>>, ParseError> {
            (_: script_section, t: _optional_text(), stmts: _multiple_statements(), t2: _optional_text(), next: _script_sections()) => {
                let mut next = try!(next);
                if let Some(text) = t2 {
                    next.push_front(ParsedItem::Text(text.into()));
                }
                next.push_front(ParsedItem::CodeBlock(try!(stmts).into_iter().collect()));
                if let Some(text) = t {
                    next.push_front(ParsedItem::Text(text.into()));
                }

                Ok(next)
            },
            () => Ok(LinkedList::new())
        }

        _optional_text(&self) -> Option<&'input str> {
            (&t: text) => Some(t),
            () => None,
        }

        _expression(&self) -> Result<Expr<'input>, ParseError> {
            (_: yield_expression, e: _yield_expression()) => e,
            (_: include_expression, _: expression, e: _expression()) => Ok(Expr::Include(IncludeTy::Include, Box::new(try!(e)))),
            (_: include_once_expression, _: expression, e: _expression()) => Ok(Expr::Include(IncludeTy::IncludeOnce, Box::new(try!(e)))),
            (_: require_expression, _: expression, e: _expression()) => Ok(Expr::Include(IncludeTy::Require, Box::new(try!(e)))),
            (_: require_once_expression, _: expression, e: _expression()) => Ok(Expr::Include(IncludeTy::RequireOnce, Box::new(try!(e)))),
            (_: expression, e: _expression()) => e,
        }

        _optional_expression(&self) -> Result<Expr<'input>, ParseError> {
            (_: expression, e: _expression()) => Ok(try!(e)),
            () => Ok(Expr::None),
        }

        _constant_expression(&self) -> Result<Expr<'input>, ParseError> {
            (_: array_creation_expression, values: _array_element_initializers()) => {
                Ok(Expr::Array(try!(values).into_iter().map(|x| (Box::new(x.0), Box::new(x.1))).collect()))
            },
            (_: expression, e: _expression()) => e,
        }

        _yield_expression(&self) -> Result<Expr<'input>, ParseError> {
            (_: assignment_expression, e: _assignment_expression()) => e,
        }

        _unary_operators(&self) -> LinkedList<UnaryOp> {
            (_: unary_operator, op, mut next: _unary_operators()) => {
                next.push_front(rule_to_unary_op(op.rule));
                next
            },
            () => LinkedList::new(),
        }

        _assignment_expression_internal(&self) -> Result<Expr<'input>, ParseError> {
            (_: simple_assignment_expression, _: unary_expression, ex: _unary_expression(), _: assignment_expression, ae: _assignment_expression()) => {
                Ok(Expr::Assign(Box::new(try!(ex)), Box::new(try!(ae))))
            },
            (_: compound_assignment_expression, _: unary_expression, ex: _unary_expression(), _: compound_assignment_operator,
                op, _: assignment_expression, ae: _assignment_expression()) => {
                Ok(Expr::CompoundAssign(Box::new(try!(ex)), rule_to_op(op.rule), Box::new(try!(ae))))
            },
            (_: byref_assignment_expression, _: unary_expression, ex: _unary_expression(), _: assignment_expression, ae: _assignment_expression()) => {
                Ok(Expr::AssignRef(Box::new(try!(ex)), Box::new(try!(ae))))
            },
            (e: _conditional_expression()) => e,
        }

        _assignment_expression(&self) -> Result<Expr<'input>, ParseError> {
            (u: _unary_operators(), e: _assignment_expression_internal()) => {
                Ok(u.into_iter().fold(try!(e), |res, op| Expr::UnaryOp(op, Box::new(res))))
            }
        }

        _conditional_expression(&self) -> Result<Expr<'input>, ParseError> {
            (_: ternary_expression, _: binary_expression, e: _binary_expression(), then: _optional_expression(), _: conditional_expression, else_: _conditional_expression()) => {
                Ok(Expr::TernaryIf(Box::new(try!(e)), Box::new(try!(then)), Box::new(try!(else_))))
            },
            (_: binary_expression, e: _binary_expression()) => e,
        }

        _binary_expression(&self) -> Result<Expr<'input>, ParseError> {
            (_: assignment_expression, e: _assignment_expression()) => e,
            (_: cast_expression, _: cast_type, ty, _: expression, e: _expression()) => {
                Ok(Expr::Cast(rule_to_ty(ty.rule), Box::new(try!(e))))
            },
            (_: unary, _: increment_or_decrement_op, op, operand: _binary_expression()) => {
                Ok(Expr::UnaryOp(rule_to_unary_op(op.rule), Box::new(try!(operand))))
            },
            (_: unary, _: unary_operator, op, operand: _binary_expression()) => {
                Ok(Expr::UnaryOp(rule_to_unary_op(op.rule), Box::new(try!(operand))))
            },
            (_: additive_expression, left: _binary_expression(), op, right: _binary_expression()) => {
                Ok(Expr::BinaryOp(rule_to_op(op.rule), Box::new(try!(left)), Box::new(try!(right))))
            },
            (_: multiplicative_expression, left: _binary_expression(), op, right: _binary_expression()) => {
                Ok(Expr::BinaryOp(rule_to_op(op.rule), Box::new(try!(left)), Box::new(try!(right))))
            },
            (_: equality_expression, left: _binary_expression(), op, right: _binary_expression()) => {
                Ok(Expr::BinaryOp(rule_to_op(op.rule), Box::new(try!(left)), Box::new(try!(right))))
            },
            (_: relational_expression, left: _binary_expression(), op, right: _binary_expression()) => {
                Ok(Expr::BinaryOp(rule_to_op(op.rule), Box::new(try!(left)), Box::new(try!(right))))
            },
            (_: logical_inc_or_expression_1, left: _binary_expression(), _: op_logical_inc_or_1, right: _binary_expression()) => {
                Ok(Expr::BinaryOp(Op::Or, Box::new(try!(left)), Box::new(try!(right))))
            },
            (_: logical_and_expression, left: _binary_expression(), _: op_and, right: _binary_expression()) => {
                Ok(Expr::BinaryOp(Op::And, Box::new(try!(left)), Box::new(try!(right))))
            },
            (_: bitwise_inc_or_expression, left: _binary_expression(), _: op_bitwise_inc_or, right: _binary_expression()) => {
                Ok(Expr::BinaryOp(Op::BitwiseInclOr, Box::new(try!(left)), Box::new(try!(right))))
            },
            (_: bitwise_exc_or_expression, left: _binary_expression(), _: op_bitwise_inc_or, right: _binary_expression()) => {
                Ok(Expr::BinaryOp(Op::BitwiseExclOr, Box::new(try!(left)), Box::new(try!(right))))
            },
            (_: exponentiation, left: _binary_expression(), _: op_pow, right: _binary_expression()) => {
                Ok(Expr::BinaryOp(Op::Pow, Box::new(try!(left)), Box::new(try!(right))))
            },
            (_: unary_expression, e: _unary_expression()) => e
        }

        _optional_instanceof_expression(&self) -> Result<Expr<'input>, ParseError> {
            (_: instanceof_expression, _: qualified_name, qn: _qualified_name()) => {
                Ok(Expr::BinaryOp(Op::Instanceof, Box::new(Expr::None), Box::new(Expr::Path(qualified_name_to_path(qn)))))
            },
            (_: instanceof_expression, _: variable_name, &name: name) => {
                Ok(Expr::BinaryOp(Op::Instanceof, Box::new(Expr::None), Box::new(Expr::Variable(name.into()))))
            },
            () => Ok(Expr::None),
        }

        _unary_expression(&self) -> Result<Expr<'input>, ParseError> {
            (_: postfix_expression, e: _postfix_expression(), o: _optional_instanceof_expression()) => {
                let o = try!(o);
                match o {
                    Expr::None => e,
                    Expr::BinaryOp(Op::Instanceof, _, ty) => Ok(Expr::BinaryOp(Op::Instanceof, Box::new(try!(e)), ty)),
                    _ => unreachable!(),
                }
            },
            (_: error_control_expression, _: expression, e: _expression()) => Ok(Expr::ErrorControl(Box::new(try!(e))))
        }

        _postfix_expression_internal(&self) -> Result<Expr<'input>, ParseError> {
            (_: primary_expression, e: _primary_expression()) => e,
            (_: ref_expression, _: assignment_expression, e: _assignment_expression()) => Ok(Expr::Reference(Box::new(try!(e)))),
            (_: object_creation_expression, _:  qualified_name, qn: _qualified_name(), args: _call_args(), _: function_call_end) => {
                Ok(Expr::New(qualified_name_to_path(qn), try!(args).into_iter().collect()))
            },
            (_: array_creation_expression, values: _array_element_initializers()) => {
                Ok(Expr::Array(try!(values).into_iter().map(|x| (Box::new(x.0), Box::new(x.1))).collect()))
            }
        }

        _array_element_initializers(&self) -> Result<LinkedList<(Expr<'input>, Expr<'input>)>, ParseError> {
            (_: array_element_initializer, kv: _array_element_key_value(), next: _array_element_initializers()) => {
                let mut next = try!(next);
                next.push_front(try!(kv));
                Ok(next)
            },
            () => Ok(LinkedList::new())
        }

        _array_element_key_value(&self) -> Result<(Expr<'input>, Expr<'input>), ParseError> {
            (_: element_key, k: _expression(), _: element_value, v: _expression()) => Ok((try!(k), try!(v))),
            (_: element_value, v: _expression()) => Ok((Expr::None, try!(v))),
        }

        _postfix_expression(&self) -> Result<Expr<'input>, ParseError> {
            (e: _postfix_expression_idxs(), _: increment_or_decrement_op, op) => {
                Ok(Expr::UnaryOp(rule_to_op_post(op.rule), Box::new(try!(e))))
            },
            (e: _postfix_expression_idxs()) => e,
        }

        // handle array index, property, fcall, static property, ...
        _postfix_expression_idxs(&self) -> Result<Expr<'input>, ParseError> {
            (pfe: _postfix_expression_internal(), pexprs: _post_exprs()) => {
                // fold the given expressions (constructing proper call/indexing expressions)
                let expr = try!(pexprs).into_iter().fold(try!(pfe), |initial, elem| {
                    match (initial, elem) {
                        (Expr::ArrayIdx(e, mut elems), IdxExpr::ArrayIdx(ai)) => {
                            elems.push(ai);
                            Expr::ArrayIdx(e, elems)
                        },
                        (Expr::ObjMember(e, mut elems), IdxExpr::ObjMember(objp)) => {
                            elems.push(objp);
                            Expr::ObjMember(e, elems)
                        },
                        (Expr::StaticMember(e, mut elems), IdxExpr::StaticMember(stp)) => {
                            elems.push(stp);
                            Expr::StaticMember(e, elems)
                        },
                        (a, IdxExpr::Call(args)) => {
                            Expr::Call(Box::new(a), args)
                        },
                        (a, IdxExpr::ArrayIdx(idx)) => {
                            Expr::ArrayIdx(Box::new(a), vec![idx])
                        },
                        (a, IdxExpr::ObjMember(objp)) => {
                            Expr::ObjMember(Box::new(a), vec![objp])
                        },
                        (a, IdxExpr::StaticMember(stp)) => {
                            Expr::StaticMember(Box::new(a), vec![stp])
                        }
                    }
                });
                Ok(expr)
            }
        }

        // general operation for anything behaving like an subscription (call, property-fetch, ...)
        // anything following a postfix_expression
        _post_exprs(&self) -> Result<LinkedList<IdxExpr<'input>>, ParseError> {
            (_: subscript, e: _optional_expression(), _: subscript_end, next: _post_exprs()) => {
                let mut next = try!(next);
                next.push_front(IdxExpr::ArrayIdx(try!(e)));
                Ok(next)
            },
            (_: function_call, args: _call_args(), _: function_call_end, next: _post_exprs()) => {
                let mut next = try!(next);
                next.push_front(IdxExpr::Call(try!(args).into_iter().collect()));
                Ok(next)
            },
            (_: member_selection, _: variable_name, &name: name, next: _post_exprs()) => {
                let mut next = try!(next);
                next.push_front(IdxExpr::ObjMember(Expr::Variable(name.into())));
                Ok(next)
            },
            (_: member_selection, &name: name, next: _post_exprs()) => {
                let mut next = try!(next);
                next.push_front(IdxExpr::ObjMember(Expr::Path(Path::Identifier(name.into()))));
                Ok(next)
            },
            (_: member_selection, _: curly_braced_expression, _: expression, e: _expression(), _: curly_braced_expression_end, next: _post_exprs()) => {
                let mut next = try!(next);
                next.push_front(IdxExpr::ObjMember(try!(e)));
                Ok(next)
            },
            // same as 3 above, for static's
            (_: scope_resolution, _: variable_name, &name: name, next: _post_exprs()) => {
                let mut next = try!(next);
                next.push_front(IdxExpr::StaticMember(Expr::Variable(name.into())));
                Ok(next)
            },
            (_: scope_resolution, &name: name, next: _post_exprs()) => {
                let mut next = try!(next);
                next.push_front(IdxExpr::StaticMember(Expr::Path(Path::Identifier(name.into()))));
                Ok(next)
            },
            (_: scope_resolution, _: curly_braced_expression, _: expression, e: _expression(), _: curly_braced_expression_end, next: _post_exprs()) => {
                let mut next = try!(next);
                next.push_front(IdxExpr::StaticMember(try!(e)));
                Ok(next)
            },
            () => Ok(LinkedList::new())
        }

        // extract call args
        _call_args(&self) -> Result<LinkedList<Expr<'input>>, ParseError> {
            (_: argument_expression, _: assignment_expression, e: _assignment_expression(), next: _call_args()) => {
                let mut next = try!(next);
                let expr = try!(e);
                next.push_front(expr);
                Ok(next)
            },
            () => Ok(LinkedList::new())
        }

        _anonym_func_use_as_ref(&self) -> bool {
            (_: use_variable_byref) => true,
            () => false,
        }

        // TODO: respect is_ref...
        _anonym_func_use_clause(&self) -> LinkedList<Cow<'input, str>> {
            (_: use_variable_name_expr, is_ref: _anonym_func_use_as_ref(), _: variable_name, &n: name, mut next: _anonym_func_use_clause()) => {
                next.push_front(n.into());
                next
            },
            () => LinkedList::new(),
        }

        _primary_expression(&self) -> Result<Expr<'input>, ParseError> {
            (_: intrinsic, i: _intrinsic()) => i,
            (_: anonym_func_creation_expression, ret_ref: _function_definition_return_ref(), params: _function_definition_params(), _: function_definition_param_end,
                use_clause: _anonym_func_use_clause(), _: compound_statement, body: _multiple_statements(), _: compound_statement_end) => {
                Ok(Expr::Function(FunctionDecl {
                    params: try!(params).into_iter().collect(),
                    body: try!(body).into_iter().collect(),
                    usev: use_clause.into_iter().collect(),
                    ret_ref: ret_ref,
                }))
            },
            (_: qualified_name, qn: _qualified_name()) => {
                Ok(Expr::Path(qualified_name_to_path(qn)))
            },
            (_: variable_name, &n: name) => Ok(Expr::Variable(n.into())),
            (_: literal, e: _literal()) => e,
            (_: constant, c: _constant()) => Ok(c),
        }

        _intrinsic(&self) -> Result<Expr<'input>, ParseError> {
            (_: list_intrinsic, e: _list_intrinsic()) => e,
            (_: echo_intrinsic, args: _multiple_expressions()) => {
                Ok(Expr::Echo(try!(args).into_iter().collect()))
            },
            (_: isset_intrinsic, args: _multiple_expressions()) => {
                Ok(Expr::Isset(try!(args).into_iter().collect()))
            },
            (_: unset_intrinsic, args: _multiple_expressions()) => {
                Ok(Expr::Unset(try!(args).into_iter().collect()))
            },
            (_: empty_intrinsic, _: expression, arg: _expression()) => {
                Ok(Expr::Empty(Box::new(try!(arg))))
            }
        }

        _list_or_variable(&self) -> Result<Expr<'input>, ParseError> {
            (_: list_intrinsic, e: _list_intrinsic()) => e,
            (_: expression, e: _expression()) => e,
        }

        _list_items(&self) -> Result<LinkedList<(Expr<'input>, Expr<'input>)>, ParseError> {
            (_: list_or_variable, e: _list_or_variable(), next: _list_items()) => {
                let mut next = try!(next);
                next.push_front((Expr::None, try!(e)));
                Ok(next)
            },
            (_: keyed_list_expression, _: expression, k: _expression(), _: list_or_variable, v: _list_or_variable(), next: _list_items()) => {
                let mut next = try!(next);
                next.push_front((try!(k), try!(v)));
                Ok(next)
            },
            (_: list_item_empty, next: _list_items()) => {
                let mut next = try!(next);
                next.push_front((Expr::None, Expr::None));
                Ok(next)
            },
            () => Ok(LinkedList::new()),
        }

        _list_intrinsic(&self) -> Result<Expr<'input>, ParseError> {
            (_: list_expression_list, items: _list_items()) => {
                Ok(Expr::List(try!(items).into_iter().collect()))
            }
        }

        // as used for compound statements
        _multiple_statements(&self) -> Result<LinkedList<Expr<'input>>, ParseError> {
            (_: statement, st: _statement(), next: _multiple_statements()) => {
                let mut next = try!(next);
                next.push_front(try!(st));
                Ok(next)
            },
            () => Ok(LinkedList::new())
        }

        _multiple_expressions(&self) -> Result<LinkedList<Expr<'input>>, ParseError> {
            (_: expression, ex: _expression(), next: _multiple_expressions()) => {
                let mut next = try!(next);
                next.push_front(try!(ex));
                Ok(next)
            },
            () => Ok(LinkedList::new())
        }

        _catch_clauses(&self) -> Result<LinkedList<CatchClause<'input>>, ParseError> {
            (_: catch_clause, _: qualified_name, qn: _qualified_name(), _: variable_name, &n: name, _: compound_statement,
                st: _multiple_statements(), _: compound_statement_end, next: _catch_clauses()) => {
                let mut next = try!(next);
                next.push_front(CatchClause {
                    ty: qualified_name_to_path(qn),
                    var: n.into(),
                    block: Expr::Block(try!(st).into_iter().collect()),
                });
                Ok(next)
            },
            () => Ok(LinkedList::new()),
        }

        _finally_clause(&self) -> Result<Expr<'input>, ParseError> {
            (_: finally_clause, _: compound_statement, st: _multiple_statements(), _: compound_statement_end) => Ok(Expr::Block(try!(st).into_iter().collect())),
            () => Ok(Expr::None),
        }

        _try_statement(&self) -> Result<Expr<'input>, ParseError> {
            (_: compound_statement, st: _multiple_statements(), _: compound_statement_end, catch_clauses: _catch_clauses(), finally_clause: _finally_clause()) => {
                Ok(Expr::Try(Box::new(Expr::Block(try!(st).into_iter().collect())), try!(catch_clauses).into_iter().collect(), Box::new(try!(finally_clause))))
            }
        }

        _statement(&self) -> Result<Expr<'input>, ParseError> {
            (_: expression_statement, e: _expression()) => e,
            (_: jump_statement, st: _jump_statement()) => st,
            (_: selection_statement, st: _selection_statement()) => st,
            (_: iteration_statement, st: _iteration_statement()) => st,
            (_: try_statement, st: _try_statement()) => st,
            (_: compound_statement, st: _multiple_statements(), _: compound_statement_end) => Ok(Expr::Block(try!(st).into_iter().collect())),
            (_: function_definition_header, ret_ref: _function_definition_return_ref(), &name: name, params: _function_definition_params(),
                _: function_definition_param_end, _: compound_statement, body: _multiple_statements(), _: compound_statement_end) => {
                Ok(Expr::Decl(Decl::GlobalFunction(name.into(), FunctionDecl {
                    params: try!(params).into_iter().collect(),
                    body: try!(body).into_iter().collect(),
                    usev: vec![],
                    ret_ref: ret_ref,
                })))
            },
            (_: class_declaration, cmod: _optional_class_modifier(), &name: name, base_clause: _class_extends(), implements: _class_implements(), members: _class_members()) => {
                Ok(Expr::Decl(Decl::Class(ClassDecl {
                    cmod: cmod,
                    name: name.into(),
                    base_class: base_clause,
                    implements: implements,
                    members: try!(members).into_iter().collect(),
                })))
            },
            (_: interface_declaration, &name: name, implements: _interface_implements(), members: _interface_members()) => {
                Ok(Expr::Decl(Decl::Interface(name.into(), implements, try!(members).into_iter().collect())))
            },
            (_: trait_declaration, &name: name, members: _trait_members()) => {
                Ok(Expr::Decl(Decl::Trait(name.into(),
                    try!(members).into_iter().collect(),
                )))
            },
            (_: namespace_definition, _: namespace_name, nsi: _namespace_name_item()) => {
                Ok(Expr::Decl(Decl::Namespace(nsi.into_iter().collect())))
            },
            (_: namespace_use_declaration, clauses: _namespace_use_clauses()) => {
                Ok(Expr::Use(clauses.into_iter().collect()))
            },
            (_: statement, st: _statement()) => st,
        }

        _class_extends(&self) -> Option<Path<'input>> {
            (_: class_base_clause, _: qualified_name, qn: _qualified_name()) => Some(qualified_name_to_path(qn)),
            () => None,
        }

        _class_implements_internal(&self) -> LinkedList<Path<'input>> {
            (_: qualified_name, qn: _qualified_name(), mut next: _class_implements_internal()) => {
                next.push_front(qualified_name_to_path(qn));
                next
            },
            () => LinkedList::new(),
        }

        _class_implements(&self) -> Vec<Path<'input>> {
            (_: class_interface_clause, i: _class_implements_internal()) => i.into_iter().collect(),
            () => vec![],
        }

        _interface_implements(&self) -> Vec<Path<'input>> {
            (_: interface_base_clause, i: _class_implements_internal()) => i.into_iter().collect(),
            () => vec![],
        }

        _trait_members(&self) -> Result<LinkedList<ClassMember<'input>>, ParseError> {
            (_: trait_member_declaration, member: _class_member(), next: _trait_members()) => {
                let mut next = try!(next);
                next.push_front(try!(member));
                Ok(next)
            },
            () => Ok(LinkedList::new())
        }

        _class_members(&self) -> Result<LinkedList<ClassMember<'input>>, ParseError> {
            (_: class_member_declaration, member: _class_member(), next: _class_members()) => {
                let mut next = try!(next);
                next.push_front(try!(member));
                Ok(next)
            },
            () => Ok(LinkedList::new())
        }

        _interface_members(&self) -> Result<LinkedList<ClassMember<'input>>, ParseError> {
            (_: interface_member_declaration, member: _class_member(), next: _trait_members()) => {
                let mut next = try!(next);
                next.push_front(try!(member));
                Ok(next)
            },
            () => Ok(LinkedList::new())
        }

        _optional_compound_statement(&self) -> Result<Vec<Expr<'input>>, ParseError> {
            (_: compound_statement, body: _multiple_statements(), _: compound_statement_end) => {
                Ok(try!(body).into_iter().collect())
            },
            () => Ok(vec![]),
        }

        _class_member(&self) -> Result<ClassMember<'input>, ParseError> {
            (_: const_declaration, &name: name, _: constant_expression, e: _constant_expression()) => {
                Ok(ClassMember::Constant(name.into(), try!(e)))
            },
            (_: property_declaration, modifiers: _modifiers(), _: variable_name, &name: name, default_value: _opt_property_value()) => {
                Ok(ClassMember::Property(modifiers, name.into(), try!(default_value)))
            },
            (_: method_declaration, modifiers: _modifiers(), _: function_definition_header, ret_ref: _function_definition_return_ref(),
                &name: name, params: _function_definition_params(), _: function_definition_param_end, body: _optional_compound_statement()) => {
                Ok(ClassMember::Method(modifiers, name.into(), FunctionDecl {
                    params: try!(params).into_iter().collect(),
                    body: try!(body),
                    usev: vec![],
                    ret_ref: ret_ref,
                }))
            },
            (_: trait_use_clause, _: trait_name_list, list: _trait_name_list(), _: trait_use_specification, clauses: _trait_select_and_alias_clauses()) =>{
                Ok(ClassMember::TraitUse(list.into_iter().collect(), clauses.into_iter().collect()))
            }
        }

        _trait_alias_as_clause(&self) -> TraitUse<'input> {
            // Dummy placeholder to make this panic, so we can implement it when required later
            // this can never happen, and this function will always panic like that
            (_: trait_alias_as_clause) => TraitUse::InsteadOf("".into(), "".into())
        }

        _trait_select_and_alias_clauses(&self) -> LinkedList<TraitUse<'input>> {
            (_: trait_select_and_alias_clause, _: trait_select_insteadof_clause, &n1: name, &n2: name, mut next: _trait_select_and_alias_clauses()) => {
                next.push_front(TraitUse::InsteadOf(n1.into(), n2.into()));
                next
            },
            (_: trait_select_and_alias_clause, _: trait_alias_as_clause, cl: _trait_alias_as_clause(), mut next: _trait_select_and_alias_clauses()) => {
                next.push_front(cl);
                next
            },
            () => LinkedList::new(),
        }

        _trait_name_list(&self) -> LinkedList<Path<'input>> {
            (_: qualified_name, qn: _qualified_name(), mut next: _trait_name_list()) => {
                next.push_front(qualified_name_to_path(qn));
                next
            },
            () => LinkedList::new(),
        }

        _opt_property_value(&self) -> Result<Expr<'input>, ParseError> {
            (_: property_initializer, _: constant_expression, e: _constant_expression()) => e,
            () => Ok(Expr::None)
        }

        _modifiers(&self) -> Modifiers {
            (_: static_modifier, mut next: _modifiers()) => {
                next.0 = true;
                next
            },
            (_: visibility_modifier, visibility, mut next: _modifiers()) => {
                next.1 = match visibility.rule {
                    Rule::visibility_private => Visibility::Private,
                    Rule::visibility_public => Visibility::Public,
                    Rule::visibility_protected => Visibility::Protected,
                    _ => unreachable!()
                };
                next
            },
            (_: class_modifier, m: _class_modifier(), mut next: _modifiers()) => {
                next.2 = m;
                next
            },
            () => Modifiers(false, Visibility::None, ClassModifier::None)
        }

        _class_modifier(&self) -> ClassModifier {
            (modifier) => match modifier.rule {
                Rule::class_modifier_final => ClassModifier::Final,
                Rule::class_modifier_abstract => ClassModifier::Abstract,
                _ => unreachable!()
            }
        }

        _optional_class_modifier(&self) -> ClassModifier {
            (_: class_modifier, m: _class_modifier()) => m,
            () => ClassModifier::None,
        }

        _namespace_aliasing_clause(&self) -> Option<Cow<'input, str>> {
            (_: namespace_aliasing_clause, &name: name) => Some(name.into()),
            () => None,
        }

        _namespace_use_clauses(&self) -> LinkedList<UseClause<'input>> {
            (_: namespace_use_clause, _: qualified_name, qn: _qualified_name(), asc: _namespace_aliasing_clause(), mut next: _namespace_use_clauses()) => {
                next.push_front(UseClause::QualifiedName(qualified_name_to_path(qn), asc));
                next
            },
            () => LinkedList::new(),
        }

        _breakout_level(&self) -> Result<usize, ParseError> {
            (_: breakout_level, _: integer_literal, val: _integer_literal()) => Ok(try!(val) as usize),
            () => Ok(1),
        }

        _jump_statement(&self) -> Result<Expr<'input>, ParseError> {
            (_: break_statement, lvl: _breakout_level()) => Ok(Expr::Break(try!(lvl))),
            (_: continue_statement, lvl: _breakout_level()) => Ok(Expr::Continue(try!(lvl))),
            (_: throw_statement, _: expression, e: _expression()) => Ok(Expr::Throw(Box::new(try!(e)))),
            (_: return_statement, _: expression, e: _expression()) => Ok(Expr::Return(Box::new(try!(e)))),
            (_: return_statement) => Ok(Expr::Return(Box::new(Expr::None))),
        }

        _iteration_statement(&self) -> Result<Expr<'input>, ParseError> {
            (_: while_statement, _: expression, e: _expression(), _: statement, st: _statement()) => {
                Ok(Expr::While(Box::new(try!(e)), Box::new(try!(st))))
            },
            (_: do_statement, _: statement, st: _statement(), _: expression, e: _expression()) => {
                Ok(Expr::DoWhile(Box::new(try!(st)), Box::new(try!(e))))
            },
            (_: foreach_statement, _: expression, e: _expression(), kv: _foreach_key_value(), st: _statement()) => {
                let kv = try!(kv);
                Ok(Expr::ForEach(Box::new(try!(e)), Box::new(kv.0), Box::new(kv.1), Box::new(try!(st))))
            }
        }

        _foreach_key_value(&self) -> Result<(Expr<'input>, Expr<'input>), ParseError> {
            (_: foreach_key, k: _expression(), _: foreach_value, v: _expression()) => Ok((try!(k), try!(v))),
            (_: foreach_value, v: _expression()) => Ok((Expr::None, try!(v))),
        }

        _case_statements(&self) -> Result<LinkedList<(Vec<Expr<'input>>, Expr<'input>)>, ParseError> {
            (_: case_statement, e: _optional_expression(), stmts: _multiple_statements(), next: _case_statements()) => {
                let mut stmts = try!(stmts);
                // convert the stmts to an appropriate expression
                let new_stmt = match stmts.len() {
                    0 => Expr::None,
                    1 => stmts.pop_front().unwrap(),
                    _ => Expr::Block(stmts.into_iter().collect()),
                };
                let e = try!(e);
                let mut next = try!(next);
                // recursion, collect all cases
                match new_stmt {
                    Expr::None if next.len() > 0 => {
                        // no body, we can merge the case into a "parent"
                        next.front_mut().unwrap().0.insert(0, e);
                    },
                    _ => next.push_front((vec![e], new_stmt)),
                }
                Ok(next)
            },
            () => Ok(LinkedList::new()),
        }

        _selection_statement(&self) -> Result<Expr<'input>, ParseError> {
            (_: switch_statement, _: expression, e: _expression(), cstmts: _case_statements()) => {
                Ok(Expr::Switch(Box::new(try!(e)), try!(cstmts).into_iter().collect()))
            },
            (_: if_statement, ifstmt: _if_statement()) => ifstmt,
        }

        _if_statement(&self) -> Result<Expr<'input>, ParseError> {
            (exp: _expression(), _: statement, st: _statement(), elsec: _else_clause()) => {
                Ok(Expr::If(Box::new(try!(exp)), Box::new(try!(st)), Box::new(try!(elsec))))
            }
        }

        _else_clause(&self) -> Result<Expr<'input>, ParseError> {
            (_: else_clause_1, st: _statement()) => Ok(try!(st)),
            () => Ok(Expr::None),
        }

        _function_definition_return_ref(&self) -> bool {
            (_: function_ret_reference) => true,
            () => false,
        }

        _function_definition_param_as_ref(&self) -> bool {
            (_: parameter_as_ref) => true,
            () => false,
        }

        _function_definition_param_ty(&self) -> Option<Ty<'input>> {
            (_: type_declaration, _: type_declaration_ty, ty) => {
                Some(rule_to_ty(ty.rule))
            },
            (_: type_declaration, _: qualified_name, qn: _qualified_name()) => {
                Some(Ty::Object(Some(qualified_name_to_path(qn))))
            },
            () => None,
        }

        _function_definition_param_default(&self) -> Result<Expr<'input>, ParseError> {
            (_: default_argument_specifier, _: constant_expression, e: _constant_expression()) => e,
            () => Ok(Expr::None),
        }

        _function_definition_params(&self) -> Result<LinkedList<ParamDefinition<'input>>, ParseError> {
            (_: parameter_declaration, ty: _function_definition_param_ty(), as_ref: _function_definition_param_as_ref(), _: variable_name,
                &name: name, default: _function_definition_param_default(), next: _function_definition_params()) => {
                let mut next = try!(next);
                next.push_front(ParamDefinition { name: name.into(), as_ref: as_ref, ty: ty, default: try!(default) });
                Ok(next)
            },
            () => Ok(LinkedList::new())
        }

        _constant(&self) -> Expr<'input> {
            (_: constant_true) => Expr::True,
            (_: constant_false) => Expr::False,
            (_: constant_null) => Expr::Null,
        }

        _literal(&self) -> Result<Expr<'input>, ParseError> {
            (_: integer_literal, e: _integer_literal()) => Ok(Expr::Int(try!(e))),
            (_: string_literal, e: _string_literal()) => e,
        }

        _integer_literal(&self) -> Result<i64, ParseError> {
            (&i: decimal_literal) => Ok(try!(i.parse())),
            (_: octal_literal, &num: octal_digit_sequence) => Ok(if num.len() > 0 { try!(i64::from_str_radix(num, 8)) } else { 0 })
        }

        _string_literal(&self) -> Result<Expr<'input>, ParseError> {
            (_: double_quoted_string_literal, str_: _dq_string()) => Ok(Expr::String(try!(str_))),
            (_: single_quoted_string_literal, str_: _sq_string()) => Ok(Expr::String(try!(str_))),
        }

        // double quoted string, handle the escaping & concatenation of all fragments (TODO: string_variable probably belongs into this)
        _dq_string(&self) -> Result<String, ParseError> {
            // handle a hex-escape sequence
            (_: dq_char, _: dq_escape_sequence, _: dq_hexadecimal_escape_sequence, &seq: hexadecimal_sequence, others: _hex_escapes(), next: _dq_string()) => {
                let mut others = try!(others);
                others.push_front(try!(u8::from_str_radix(seq, 16)));
                Ok(try!(String::from_utf8(others.into_iter().collect())) + &try!(next))
            },
            // handle a octal-escape sequence
            (_: dq_char, _: dq_escape_sequence, _: dq_octal_escape_sequence, &seq: octal_sequence, next: _dq_string()) => {
                Ok(try!(String::from_utf8(vec![try!(u8::from_str_radix(seq, 8))])) + &try!(next))
            },
            // handle an escape-sequence
            (_: dq_char, _: dq_escape_sequence, &es: dq_simple_escape_sequence, next: _dq_string()) => {
                Ok(match es {
                    "\\n" => "\n",
                    "\\r" => "\r",
                    "\\t" => "\t",
                    "\\\"" => "\"",
                    "\\\'" => "\'",
                    "\\\\" => "\\",
                    _ => unreachable!()
                }.to_owned() + &try!(next))
            },
            // handle a character
            (&ch: dq_char, next: _dq_string()) => {
                Ok(ch.to_owned() + &try!(next))
            },
            () => Ok(String::new())
        }

        // a singled-quoted string, ignoring most of escaping, raw-literal like
        _sq_string(&self) -> Result<String, ParseError> {
            (_: sq_char, &es: sq_escape_sequence, next: _sq_string()) => {
                Ok(match es {
                    "\\\'" => "'",
                    "\\\\" => "\\",
                    es => es,
                }.to_owned() + &try!(next))
            },
            (&ch: sq_char, next: _sq_string()) => {
                Ok(ch.to_owned() + &try!(next))
            },
            () => Ok(String::new())
        }

        // handle the collection of bytes to later convert UTF8-bytes from hex-escapes into a string
        _hex_escapes(&self) -> Result<LinkedList<u8>, ParseError> {
            (_: dq_char, _: dq_escape_sequence, _: dq_hexadecimal_escape_sequence, &current: hexadecimal_sequence, next: _hex_escapes()) => {
                let mut next = try!(next);
                next.push_front(try!(u8::from_str_radix(current, 16)));
                Ok(next)
            },
            () => Ok(LinkedList::new())
        }

        _qualified_name(&self) -> Vec<Cow<'input, str>> {
            (_: namespace_name_as_a_prefix, ns: _namespace_name_as_prefix(), &last: name) => {
                let mut result = ns;
                result.push_back(last.into());
                result.into_iter().collect()
            },
            (&current: name) => vec![current.into()],
        }

        _namespace_name_as_prefix(&self) -> LinkedList<Cow<'input, str>> {
            (e: _namespace_name_item()) => e,
        }

        _namespace_name_item(&self) -> LinkedList<Cow<'input, str>> {
            (_: namespace_name_item, &current: name, mut next: _namespace_name_item()) => {
                next.push_front(current.into());
                next
            },
            () => LinkedList::new()
        }
    }
}

fn qualified_name_to_path<'a>(mut args: Vec<Cow<'a, str>>) -> Path<'a> {
    let class_fragment = args.pop().unwrap();
    if args.len() > 0 {
        let namespace = args.join("\\");
        Path::NsIdentifier(namespace.into(), class_fragment.into())
    } else {
        Path::Identifier(class_fragment.into())
    }
}

fn rule_to_ty<'a>(r: Rule) -> Ty<'a> {
    match r {
        Rule::type_array => Ty::Array,
        Rule::type_string => Ty::String,
        Rule::type_int | Rule::type_integer => Ty::Int,
        Rule::type_float => Ty::Float,
        Rule::type_bool | Rule::type_boolean => Ty::Bool,
        Rule::type_double => Ty::Double,
        Rule::type_callable => Ty::Callable,
        Rule::type_object => Ty::Object(None),
        //TODO
        Rule::type_binary => unimplemented!(),
        Rule::type_real => unimplemented!(),
        Rule::type_unset => unimplemented!(),
        _ => unreachable!(),
    }
}

fn rule_to_op(r: Rule) -> Op {
    // all Rules starting with ```compound_op_``` can only occur in a compound assignment instruction, we simplify the internals
    // by mapping them to the same rule e.g. compound_op_add -> Op::Add
    match r {
        Rule::op_concat | Rule::compound_op_concat => Op::Concat,
        Rule::op_add | Rule::compound_op_add => Op::Add,
        Rule::op_sub | Rule::compound_op_sub => Op::Sub,
        Rule::op_mul | Rule::compound_op_mul => Op::Mul,
        Rule::op_div | Rule::compound_op_div => Op::Div,
        Rule::op_mod | Rule::compound_op_mod => Op::Mod,
        Rule::op_equality_identical => Op::Identical,
        Rule::op_equality_not_identical => Op::NotIdentical,
        Rule::op_equality_eq => Op::Eq,
        Rule::op_equality_neq => Op::Neq,
        Rule::op_equality_uneq => Op::Uneq,
        Rule::op_lt => Op::Lt,
        Rule::op_gt => Op::Gt,
        Rule::op_le => Op::Le,
        Rule::op_ge => Op::Ge,
        Rule::op_spaceship => Op::Spaceship,
        _ => unreachable!(),
    }
}

fn rule_to_unary_op(r: Rule) -> UnaryOp {
    match r {
        Rule::op_not => UnaryOp::Not,
        Rule::op_add => UnaryOp::Negative,
        Rule::op_bitwise_not => UnaryOp::BitwiseNot,
        Rule::op_sub => UnaryOp::Positive,
        Rule::op_increment => UnaryOp::PreInc,
        Rule::op_decrement => UnaryOp::PreDec,
        _ => unreachable!(),
    }
}

fn rule_to_op_post(r: Rule) -> UnaryOp {
    match r {
        Rule::op_increment => UnaryOp::PostInc,
        Rule::op_decrement => UnaryOp::PostDec,
        r => rule_to_unary_op(r),
    }
}

// HELPERS; TODO: move to tests files when visibility is added for these macros
pub fn process_script(input: &str) -> Vec<ParsedItem> {
    let mut parser = Rdp::new(StringInput::new(input));
    if !parser.script() {
        println!("{:?}", parser.input().line_col(parser.input().pos()));
        panic!("failed");
    }
    //println!("{:?} @{}", parser.queue(), parser.input().pos());
    println!("{:?}", parser.input().line_col(parser.input().pos()));
    assert!(parser.end());
    parser.main().unwrap()
}

pub fn process_stmt(input: &str) -> Expr {
    let mut parser = Rdp::new(StringInput::new(input));
    assert!(parser.statement());
    println!("{:?} @{}", parser.queue(), parser.input().pos());
    assert!(parser.end());
    parser._statement().unwrap()
}

pub fn process_expr(input: &str) -> Expr {
    let mut parser = Rdp::new(StringInput::new(input));
    assert!(parser.expression());
    println!("{:?} @{}", parser.queue(), parser.input().pos());
    assert!(parser.end());
    parser._expression().unwrap()
}
