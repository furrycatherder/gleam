use super::{
    BitArrayOption, BitArraySegment, CallArg, Clause, RecordBeingUpdated, Statement, UntypedExpr,
    UntypedRecordUpdateArg, UntypedStatement,
};

type Cont<'a> = fn(&UntypedExpr) -> UntypedExpr;

pub fn to_cps(expr: &UntypedExpr, k: &Cont<'_>) -> UntypedExpr {
    match expr {
        // Atomic expressions
        UntypedExpr::Int { .. }
        | UntypedExpr::Float { .. }
        | UntypedExpr::String { .. }
        | UntypedExpr::Var { .. } => cps_atom(expr, k),

        // Block expressions
        UntypedExpr::Block { .. } => cps_block(expr, k),

        // Collection expressions
        UntypedExpr::List { .. } => cps_list(expr, k),
        UntypedExpr::Tuple { .. } => cps_tuple(expr, k),
        UntypedExpr::BitArray { .. } => cps_bit_array(expr, k),

        // Control flow expressions
        UntypedExpr::Case { .. } => cps_case(expr, k),
        UntypedExpr::Fn { .. } => cps_fn(expr, k),

        // Operation expressions
        UntypedExpr::Call { .. } => cps_call(expr, k),
        UntypedExpr::BinOp { .. } => cps_bin_op(expr, k),
        UntypedExpr::PipeLine { .. } => cps_pipe_line(expr, k),

        // Access expressions
        UntypedExpr::FieldAccess { .. } => cps_field_access(expr, k),
        UntypedExpr::TupleIndex { .. } => cps_tuple_index(expr, k),

        // Update expressions
        UntypedExpr::RecordUpdate { .. } => cps_record_update(expr, k),

        // Unary operations
        UntypedExpr::NegateBool { .. } => cps_negate_bool(expr, k),
        UntypedExpr::NegateInt { .. } => cps_negate_int(expr, k),

        // Side effect expressions
        UntypedExpr::Todo { .. } => cps_todo(expr, k),
        UntypedExpr::Panic { .. } => cps_panic(expr, k),
        UntypedExpr::Echo { .. } => cps_echo(expr, k),

        // Other
        UntypedExpr::Placeholder { location } => {
            // Placeholder is handled directly
            k(&UntypedExpr::Placeholder {
                location: *location,
            })
        }
    }
}

fn cps_statement(statement: &UntypedStatement, k: &Cont<'_>) -> UntypedStatement {
    match statement {
        Statement::Expression(expr) => Statement::Expression(to_cps(expr, k)),
        Statement::Assignment(assignment) => Statement::Assignment(super::Assignment {
            value: Box::new(to_cps(&assignment.value, k)),
            ..assignment.clone()
        }),
        Statement::Use(use_stmt) => Statement::Use(super::Use {
            call: Box::new(to_cps(&use_stmt.call, k)),
            ..use_stmt.clone()
        }),
    }
}

fn cps_atom(expr: &UntypedExpr, k: &Cont<'_>) -> UntypedExpr {
    k(expr)
}

fn cps_fn(expr: &UntypedExpr, k: &Cont<'_>) -> UntypedExpr {
    match expr {
        UntypedExpr::Fn {
            kind,
            end_of_head_byte_index,
            arguments,
            body,
            return_annotation,
            ..
        } => {
            let go_arg = super::UntypedArg {
                location: expr.location(),
                type_: (),
                names: super::ArgNames::Named {
                    name: "go".into(),
                    location: expr.location(),
                },
                annotation: None,
            };

            let body_k: Cont<'_> = |stmt: &UntypedExpr| UntypedExpr::Call {
                fun: Box::new(UntypedExpr::Var {
                    location: stmt.location(),
                    name: "go".into(),
                }),
                arguments: vec![CallArg {
                    location: stmt.location(),
                    value: stmt.clone(),
                    label: None,
                    implicit: None,
                }],
                location: stmt.location(),
            };

            let (init, last) = body.to_owned().split_off_last();
            let body_cps = cps_statement(&last, &body_k);

            let fn_expr = UntypedExpr::Fn {
                location: expr.location(),
                kind: *kind,
                end_of_head_byte_index: *end_of_head_byte_index,
                arguments: vec1::Vec1::from_vec_push(arguments.to_owned(), go_arg).to_vec(),
                body: vec1::Vec1::from_vec_push(init, body_cps),
                return_annotation: return_annotation.to_owned(),
            };

            k(&fn_expr)
        }
        _ => unreachable!(),
    }
}

fn cps_list(expr: &UntypedExpr, k: &Cont<'_>) -> UntypedExpr {
    match expr {
        UntypedExpr::List { elements, tail, .. } => {
            let list = UntypedExpr::List {
                location: expr.location(),
                elements: elements.iter().map(|elem| to_cps(elem, k)).collect(),
                tail: tail.as_ref().map(|tail| Box::new(to_cps(tail, k))),
            };

            k(&list)
        }
        _ => unreachable!(),
    }
}

fn cps_call(expr: &UntypedExpr, k: &Cont<'_>) -> UntypedExpr {
    match expr {
        UntypedExpr::Call { fun, arguments, .. } => {
            let call = UntypedExpr::Call {
                location: expr.location(),
                fun: Box::new(to_cps(fun, k)),
                arguments: arguments
                    .iter()
                    .map(|arg| CallArg {
                        value: to_cps(&arg.value, k),
                        ..arg.clone()
                    })
                    .collect(),
            };

            k(&call)
        }
        _ => unreachable!(),
    }
}

fn cps_bin_op(expr: &UntypedExpr, k: &Cont<'_>) -> UntypedExpr {
    match expr {
        UntypedExpr::BinOp {
            name, left, right, ..
        } => {
            let bin_op = UntypedExpr::BinOp {
                location: expr.location(),
                name: *name,
                left: Box::new(to_cps(left, k)),
                right: Box::new(to_cps(right, k)),
            };

            k(&bin_op)
        }
        _ => unreachable!(),
    }
}

fn cps_pipe_line(expr: &UntypedExpr, k: &Cont<'_>) -> UntypedExpr {
    match expr {
        UntypedExpr::PipeLine { expressions, .. } => {
            let pipe_line = UntypedExpr::PipeLine {
                expressions: expressions.clone().mapped(|expr| to_cps(&expr, k)),
            };

            k(&pipe_line)
        }
        _ => unreachable!(),
    }
}

fn cps_case(expr: &UntypedExpr, k: &Cont<'_>) -> UntypedExpr {
    match expr {
        UntypedExpr::Case {
            subjects, clauses, ..
        } => {
            let case = UntypedExpr::Case {
                location: expr.location(),
                subjects: subjects.iter().map(|subject| to_cps(subject, k)).collect(),
                clauses: clauses.as_ref().map(|clauses| {
                    clauses
                        .iter()
                        .map(|clause| Clause {
                            then: to_cps(&clause.then, k),
                            ..clause.clone()
                        })
                        .collect()
                }),
            };

            k(&case)
        }
        _ => unreachable!(),
    }
}

fn cps_field_access(expr: &UntypedExpr, k: &Cont<'_>) -> UntypedExpr {
    match expr {
        UntypedExpr::FieldAccess {
            label_location,
            label,
            container,
            ..
        } => {
            let field_access = UntypedExpr::FieldAccess {
                location: expr.location(),
                label: label.clone(),
                label_location: *label_location,
                container: Box::new(to_cps(container, k)),
            };

            k(&field_access)
        }
        _ => unreachable!(),
    }
}

fn cps_tuple(expr: &UntypedExpr, k: &Cont<'_>) -> UntypedExpr {
    match expr {
        UntypedExpr::Tuple { elems, .. } => {
            let tuple = UntypedExpr::Tuple {
                location: expr.location(),
                elems: elems.iter().map(|elem| to_cps(elem, k)).collect(),
            };

            k(&tuple)
        }
        _ => unreachable!(),
    }
}

fn cps_tuple_index(expr: &UntypedExpr, k: &Cont<'_>) -> UntypedExpr {
    match expr {
        UntypedExpr::TupleIndex { index, tuple, .. } => {
            let tuple_index = UntypedExpr::TupleIndex {
                location: expr.location(),
                index: *index,
                tuple: Box::new(to_cps(tuple, k)),
            };

            k(&tuple_index)
        }
        _ => unreachable!(),
    }
}

fn cps_block(expr: &UntypedExpr, k: &Cont<'_>) -> UntypedExpr {
    match expr {
        UntypedExpr::Block { statements, .. } => {
            let block = UntypedExpr::Block {
                location: expr.location(),
                statements: statements.clone().mapped(|stmt| cps_statement(&stmt, k)),
            };

            k(&block)
        }
        _ => unreachable!(),
    }
}

fn cps_todo(expr: &UntypedExpr, k: &Cont<'_>) -> UntypedExpr {
    match expr {
        UntypedExpr::Todo { kind, message, .. } => {
            let todo = UntypedExpr::Todo {
                kind: *kind,
                location: expr.location(),
                message: message.as_ref().map(|message| Box::new(to_cps(message, k))),
            };

            k(&todo)
        }
        _ => unreachable!(),
    }
}

fn cps_panic(expr: &UntypedExpr, k: &Cont<'_>) -> UntypedExpr {
    match expr {
        UntypedExpr::Panic { message, .. } => {
            let panic = UntypedExpr::Panic {
                location: expr.location(),
                message: message.as_ref().map(|message| Box::new(to_cps(message, k))),
            };

            k(&panic)
        }
        _ => unreachable!(),
    }
}

fn cps_echo(expr: &UntypedExpr, k: &Cont<'_>) -> UntypedExpr {
    match expr {
        UntypedExpr::Echo { expression, .. } => {
            let echo = UntypedExpr::Echo {
                location: expr.location(),
                expression: expression
                    .as_ref()
                    .map(|expression| Box::new(to_cps(expression, k))),
            };

            k(&echo)
        }
        _ => unreachable!(),
    }
}

fn cps_bit_array_option(
    option: &BitArrayOption<UntypedExpr>,
    k: &Cont<'_>,
) -> BitArrayOption<UntypedExpr> {
    match option {
        BitArrayOption::Size {
            value,
            short_form,
            location,
        } => BitArrayOption::Size {
            location: *location,
            short_form: *short_form,
            value: Box::new(to_cps(value.as_ref(), k)),
        },
        _ => option.clone(),
    }
}

fn cps_bit_array(expr: &UntypedExpr, k: &Cont<'_>) -> UntypedExpr {
    match expr {
        UntypedExpr::BitArray { segments, .. } => {
            let bit_array = UntypedExpr::BitArray {
                location: expr.location(),
                segments: segments
                    .iter()
                    .map(|segment| BitArraySegment {
                        value: Box::new(to_cps(&segment.value, k)),
                        options: segment
                            .options
                            .iter()
                            .map(|option| cps_bit_array_option(option, k))
                            .collect(),
                        ..segment.clone()
                    })
                    .collect(),
            };

            k(&bit_array)
        }
        _ => unreachable!(),
    }
}

fn cps_record_being_updated(record: &RecordBeingUpdated, k: &Cont<'_>) -> RecordBeingUpdated {
    RecordBeingUpdated {
        base: Box::new(to_cps(record.base.as_ref(), k)),
        location: record.location,
    }
}

fn cps_record_update_arg(arg: &UntypedRecordUpdateArg, k: &Cont<'_>) -> UntypedRecordUpdateArg {
    UntypedRecordUpdateArg {
        value: to_cps(&arg.value, k),
        ..arg.clone()
    }
}

fn cps_record_update(expr: &UntypedExpr, k: &Cont<'_>) -> UntypedExpr {
    match expr {
        UntypedExpr::RecordUpdate {
            constructor,
            record,
            arguments,
            ..
        } => {
            let record_update = UntypedExpr::RecordUpdate {
                location: expr.location(),
                constructor: Box::new(to_cps(constructor, k)),
                record: cps_record_being_updated(record, k),
                arguments: arguments
                    .iter()
                    .map(|arg| cps_record_update_arg(arg, k))
                    .collect(),
            };

            k(&record_update)
        }
        _ => unreachable!(),
    }
}

fn cps_negate_bool(expr: &UntypedExpr, k: &Cont<'_>) -> UntypedExpr {
    match expr {
        UntypedExpr::NegateBool { value, .. } => {
            let negate_bool = UntypedExpr::NegateBool {
                location: expr.location(),
                value: Box::new(to_cps(value, k)),
            };

            k(&negate_bool)
        }
        _ => unreachable!(),
    }
}

fn cps_negate_int(expr: &UntypedExpr, k: &Cont<'_>) -> UntypedExpr {
    match expr {
        UntypedExpr::NegateInt { value, .. } => {
            let negate_int = UntypedExpr::NegateInt {
                location: expr.location(),
                value: Box::new(to_cps(value, k)),
            };

            k(&negate_int)
        }
        _ => unreachable!(),
    }
}
