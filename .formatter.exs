# Used by "mix format"
[
  inputs: ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}"],
  locals_without_parens: [spectest: 1],
  import_depts: [:stream_data, :type_check]
]
