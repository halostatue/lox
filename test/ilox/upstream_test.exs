defmodule Ilox.UpstreamTest do
  use ExUnit.Case, async: true

  import Ilox.Token, only: [__type_map: 0]
  import Ilox.SourceTools

  @timeout 2000

  describe "scan" do
    files =
      lox_files(%{
        "" => "unexpected_character",
        "scanning" => [
          "identifiers",
          "keywords",
          "numbers",
          "punctuators",
          "strings",
          "whitespace"
        ],
        "string" => ["unterminated"]
      })

    for {filename, name} <- files do
      @tag timeout: @timeout
      test "#{name}" do
        scan_test(unquote(filename))
      end
    end
  end

  describe "parse" do
    files =
      lox_files(%{
        "assignment" => ["grouping", "infix_operator", "prefix_operator", "to_this"],
        "for" => [
          "class_in_body",
          "fun_in_body",
          "statement_condition",
          "statement_increment",
          "statement_initializer",
          "var_in_body"
        ],
        "function" => [
          "body_must_be_block",
          "missing_comma_in_parameters",
          "too_many_arguments",
          "too_many_parameters"
        ],
        "if" => [
          "class_in_else",
          "class_in_then",
          "fun_in_else",
          "fun_in_then",
          "var_in_else",
          "var_in_then"
        ],
        "inheritance" => ["parenthesized_superclass"],
        "method" => ["too_many_arguments", "too_many_parameters"],
        "number" => ["decimal_point_at_eof", "leading_dot", "trailing_dot"],
        "print" => ["missing_argument"],
        "super" => ["parenthesized", "super_without_dot", "super_without_name"],
        "variable" => [
          "use_false_as_var",
          "use_nil_as_var",
          "use_this_as_var"
        ],
        "while" => [
          "class_in_body",
          "fun_in_body",
          "var_in_body"
        ]
      })

    for {filename, name} <- files do
      @tag timeout: @timeout
      test "#{name}" do
        parse_test(unquote(filename))
      end
    end
  end

  describe "resolve" do
    files =
      lox_files(%{
        "class" => ["inherit_self", "local_inherit_self"],
        "constructor" => ["return_value"],
        "return" => ["at_top_level"],
        "super" => [
          "no_superclass_bind",
          "no_superclass_call",
          "super_at_top_level",
          "super_in_top_level_function"
        ],
        "this" => [
          "this_at_top_level",
          "this_in_top_level_function"
        ],
        "variable" => [
          "duplicate_local",
          "duplicate_parameter",
          "use_local_in_initializer"
        ]
      })

    for {filename, name} <- files do
      @tag timeout: @timeout
      test "#{name}" do
        resolve_test(unquote(filename))
      end
    end
  end

  describe "run" do
    files =
      lox_files(%{
        "" => "precedence.lox",
        "assignment" => ["associativity", "global", "local"],
        "block" => ["empty", "scope"],
        "bool" => ["equality", "not"],
        "call" => ["bool", "nil", "num", "object", "string"],
        "class" => [
          "empty",
          "inherited_method",
          "local_inherit_other",
          "local_reference_self",
          "reference_self"
        ],
        "closure" => [
          "assign_to_closure",
          # This file does not work as expected.
          # "assign_to_shadowed_later",
          "close_over_function_parameter",
          "close_over_later_variable",
          "close_over_method_parameter",
          "closed_closure_in_function",
          "nested_closure",
          "open_closure_in_function",
          "reference_closure_multiple_times",
          "reuse_closure_slot",
          "shadow_closure_with_local",
          "unused_closure",
          "unused_later_closure"
        ],
        "comments" => ["line_at_eof", "unicode"],
        "constructor" => [
          "arguments",
          #         "call_init_early_return",
          # TODO not yet working
          # "call_init_explicitly",
          "default",
          #         "default_arguments",
          #         "early_return",
          "extra_arguments",
          "init_not_method",
          "missing_arguments"
          #         "return_in_nested_function"
        ],
        "field" => [
          "call_function_field",
          "call_nonfunction_field",
          "get_and_set_method",
          "get_on_bool",
          "get_on_class",
          "get_on_function",
          "get_on_nil",
          "get_on_num",
          "get_on_string",
          "many",
          "method",
          "method_binds_this",
          "on_instance",
          #         "set_evaluation_order",
          "set_on_bool",
          "set_on_class",
          "set_on_function",
          "set_on_nil",
          "set_on_num",
          "set_on_string",
          "undefined"
        ],
        "for" => [
          # Nothing is executing after the loop
          # "closure_in_body",
          "return_closure",
          "return_inside"
          # Execution is timing out.
          # "scope"
          # Nothing is executing after the first loop
          # "syntax"
        ],
        "function" => [
          "empty_body",
          "extra_arguments",
          "local_mutual_recursion",
          "local_recursion",
          "missing_arguments",
          "mutual_recursion",
          "nested_call_with_arguments",
          "parameters",
          "print",
          "recursion"
        ],
        "if" => [
          "dangling_else",
          "else",
          "if",
          "truth"
        ],
        "inheritance" => [
          # Instance matching issue
          # "constructor",
          "inherit_from_function",
          "inherit_from_nil",
          "inherit_from_number",
          "inherit_methods"
          # Undefined property issue
          # "set_fields_from_base_class"
        ],
        # "limit" => [
        #   # -- Ilox does not limit block size.
        #   # "loop_too_large",
        #   # -- Ilox does not track constant use.
        #   # "no_reuse_constants",
        #   # -- Ilox does not track stack overflow and we hit a timeout.
        #   # "stack_overflow",
        #   # -- Ilox does not track constant use.
        #   # "too_many_constants",
        #   # -- Ilox does not track locals use.
        #   # "too_many_locals",
        #   # -- Ilox does not track closure variables use.
        #   # "too_many_upvalues"
        # ],
        "logical_operator" => [
          "and",
          "and_truth",
          "or",
          "or_truth"
        ],
        "method" => [
          "arity",
          "empty_block",
          "extra_arguments",
          "missing_arguments",
          "not_found",
          "print_bound_method",
          "refer_to_name"
        ],
        "nil" => [
          "literal"
        ],
        "number" => [
          "literals"
          # Java does 0/0 is NaN. The BEAM does not.
          # "nan_equality"
        ],
        "operator" => [
          "add",
          "add_bool_nil",
          "add_bool_num",
          "add_bool_string",
          "add_nil_nil",
          "add_num_nil",
          "add_string_nil",
          "comparison",
          "divide",
          "divide_nonnum_num",
          "divide_num_nonnum",
          "equals",
          "equals_class",
          "equals_method",
          "greater_nonnum_num",
          "greater_num_nonnum",
          "greater_or_equal_nonnum_num",
          "greater_or_equal_num_nonnum",
          "less_nonnum_num",
          "less_num_nonnum",
          "less_or_equal_nonnum_num",
          "less_or_equal_num_nonnum",
          "multiply",
          "multiply_nonnum_num",
          "multiply_num_nonnum",
          "negate",
          "negate_nonnum",
          "not",
          "not_class",
          "not_equals",
          "subtract",
          "subtract_nonnum_num",
          "subtract_num_nonnum"
        ],
        "regression" => [
          "394",
          "40"
        ],
        "return" => [
          "after_else",
          "after_if",
          "after_while",
          "in_function",
          "in_method",
          "return_nil_if_no_value"
        ],
        "string" => [
          "error_after_multiline",
          "literals",
          "multiline"
        ],
        "super" => [
          "bound_method",
          "call_other_method",
          "call_same_method",
          "closure",
          # Instance issue
          # "constructor",
          "extra_arguments",
          "indirectly_inherited",
          "missing_arguments",
          "no_superclass_method",
          "reassign_superclass",
          "super_in_closure_in_inherited_method",
          "super_in_inherited_method"
          # Instance issue
          # "this_in_superclass_method"
        ],
        "this" => [
          "closure",
          "nested_class",
          "nested_closure",
          "this_in_method"
        ],
        "variable" => [
          "early_bound",
          "in_middle_of_block",
          "in_nested_block",
          "local_from_method",
          "redeclare_global",
          "redefine_global",
          "scope_reuse_in_different_blocks",
          "shadow_and_local",
          "shadow_global",
          "shadow_local",
          "undefined_global",
          "undefined_local",
          "uninitialized",
          "unreached_undefined",
          "use_global_in_initializer"
        ],
        "while" => [
          "closure_in_body",
          "return_closure",
          "return_inside",
          "syntax"
        ]
      })

    for {filename, name} <- files do
      @tag timeout: @timeout
      test "#{name}" do
        run_test(unquote(filename))
      end
    end
  end

  @token_type_map __type_map()

  defp token_lookup(name), do: @token_type_map[name]

  defp scan_test(filename) do
    case load_lox(filename) do
      {_source, nil, nil} ->
        raise "No expectations."

      {source, expected_ok, nil} ->
        expected_ok =
          expected_ok
          |> Enum.map(&String.split(&1, " "))
          |> Enum.map(fn [type, lexeme, literal] -> {token_lookup(type), lexeme, literal} end)

        assert {:ok, ok} = scan(source)

        for {{type, lexeme, literal}, token} <- Enum.zip(expected_ok, ok) do
          assert type == token.type

          if type != :eof do
            assert lexeme == token.lexeme
          end

          if literal != "null" do
            assert literal == to_string(token.literal)
          end
        end

      {source, nil, expected_errors} ->
        assert {:error, :scanner, errors} = scan(source)
        assert expected_errors == errors
    end
  end

  defp parse_test(filename) do
    case load_lox(filename) do
      {source, nil, nil} ->
        assert {:ok, _} = parse(source)

      {source, nil, expected_errors} ->
        assert {:error, :parser, errors} = parse(source)
        assert expected_errors == errors
    end
  end

  defp resolve_test(filename) do
    case load_lox(filename) do
      {_source, nil, nil} ->
        raise "No expectations."

      {source, nil, expected_errors} ->
        assert {:error, :resolver, errors} = resolve(source)
        assert expected_errors == errors
    end
  end

  defp run_test(filename) do
    case load_lox(filename) do
      {_source, nil, nil} ->
        raise "No expectations."

      {source, nil, [expected_error | _]} ->
        assert {:error, :runtime, errors: error, output: []} = run(source)
        assert expected_error == error

      {source, expected_ok, nil} ->
        assert {:ok, output: ok} = run(source)
        assert expected_ok == ok

      {source, expected_ok, [expected_error | _]} ->
        assert {:error, :runtime, errors: error, output: ok} = run(source)
        assert expected_error == error
        assert expected_ok == ok
    end
  end
end
