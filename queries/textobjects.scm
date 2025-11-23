;; Functions
(define_function
    (function_definition
        body: (_
            "{"
            (_)* @function.inside
            "}" ))
) @function.around

;; Structs
(struct_specifier
    body: (_
        "{"
        (_)* @class.inside
        "}")
) @class.around

;; Comments
(comment) @comment.around
