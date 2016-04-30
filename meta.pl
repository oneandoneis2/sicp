#!/usr/bin/env perl

# The SICP evaluator, in Perl
use feature 'say';

sub myeval {
    my ($exp, $env) = @_;
    if (is_self_evaluating($exp)) { $exp }
    elsif (is_variable $exp) { lookup_variable_value( $exp, $env) }
    elsif (is_quoted $exp) { text_of_quotation($exp) }
    elsif (is_assignment $exp) { eval_assignment($exp, $env) }
    elsif (is_definition $exp) { eval_definition($exp, $env) }
    elsif (is_if $exp) { eval_if($exp, $env) }
    elsif (is_lambda $exp) {
        make_procedure(lambda_params($exp), lambda_body($exp), $env)
    }
    elsif (is_begin $exp) { eval_sequence( begin_actions($exp), $env) }
    elsif (is_cond $exp) { myeval( cond_to_if($exp), $env ) }
    elsif (is_application $exp) {
        apply(
            myeval( operator($exp), $env),
            list_of_values(operands($exp), $env)
        )
    }
    else { error("Unknown expressions type -- EVAL", $exp) }
}

sub apply {
    my ($procedure, $arguments) = @_;
    if (is_primitive_procedure $procedure) {
        apply_primitive_procedure($procedure, $arguments)
    }
    elsif (is_compound_procedure $procedure) {
        eval_sequence(
            procedure_body($procedure),
            extend_environment(
                procedure_params($procedure),
                $arguments,
                procedure_environment($procedure)
            )
        )
    }
    else {
        error("Unknown procedure type -- APPLY", $procedure)
    }
}

sub list_of_values {
    my ($exps, $env) = @_;

    if (is_no_operands $exps) {
        undef
    }
    else {
        cons(
            myeval( first_operand($exps), $env ),
            list_of_values( rest_operands($exps), $env)
            )
    }
}

sub cons {
    my ($head, $tail) = @_;
    return sub { my $op = shift; $op->( $head, $tail ) }
}

sub car {
    my $cons = shift;
    return $cons->( sub { my ($head, $tail) = @_; return $head } )
}

sub cdr {
    my $cons = shift;
    return $cons->( sub { my ($head, $tail) = @_; return $tail } )
}

sub list {
    if (@_) {
        my ($head, @tail) = @_;
        cons( $head, list(@tail) )
    }
    else {
        undef
    }
}
