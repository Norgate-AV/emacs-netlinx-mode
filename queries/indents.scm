[
  (if_statement)
  (for_statement)
  (while_statement)
  (switch_statement)
  (case_statement)
  (else_clause)
  (select_statement)
  (active_statement)
] @indent

(_ "{" "}" @end) @indent
(_ "(" ")" @end) @indent

(if_statement) @start.if
(for_statement) @start.for
(while_statement) @start.while
(switch_statement) @start.switch
(case_statement) @start.case
(else_clause) @start.else
(select_statement) @start.select
(active_statement) @start.active
