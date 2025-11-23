(
    [
        (program_name)
        (module_name)
    ] @context
) @item

(preproc_define
    (preproc_define_keyword) @context
    name: (_) @name
) @item

(preproc_include
    (preproc_include_keyword) @context
    path: (_) @name
) @item

(preproc_warn
    (preproc_warn_keyword) @context
    message: (_) @name
) @item

(struct_specifier
    (struct_keyword) @context
    name: (_) @name
) @item

(define_function
    (define_function_keyword) @context
    (function_definition
        return_type: (_)? @context
        name: (_) @name
        parameters: (parameter_list
            "(" @context
            ")" @context))
) @item

(section
    (_) @name
) @item

(define_module
    (define_module_keyword) @context
    (module_definition
        module_name: (_) @name
        instance_name: (_) @context
        parameters: (argument_list
            "(" @context
            ")" @context))
) @item

(
    [
        (timeline_event_declarator)
        (button_event_declarator)
        (channel_event_declarator)
        (level_event_declarator)
        (custom_event_declarator)
        (data_event_declarator)
    ] @context
    body: (_
        "{" @context
        "}" @context)
) @item

(comment) @annotation
