use crate::types::*;

#[derive(Clone, Debug)]
struct TokenStream {
    tokens: Vec<String>,
    position: usize,
}

impl TokenStream {
    fn new(tokens: Vec<String>) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    fn peek(&self) -> Option<&str> {
        self.tokens.get(self.position).map(|s| s.as_str())
    }

    fn advance(&mut self) -> Option<String> {
        if self.position >= self.tokens.len() {
            None
        } else {
            let token = self.tokens[self.position].clone();
            self.position += 1;
            Some(token)
        }
    }

    fn consume(&mut self, expected_token: &str) -> bool {
        if self.peek() == Some(expected_token) {
            self.position += 1;
            true
        } else {
            false
        }
    }
}

pub fn parse_kas_program(source_code: &str) -> Result<Vec<Statement>, String> {
    let source_lines: Vec<&str> = source_code.lines().collect();
    let mut line_index = 0usize;
    let mut parsed_statements: Vec<Statement> = Vec::new();

    while line_index < source_lines.len() {
        let current_line = source_lines[line_index]
            .trim_start_matches('\u{feff}')
            .trim();
        line_index += 1;

        if current_line.is_empty() || current_line.starts_with('#') {
            parsed_statements.push(Statement::EmptyStatement);
            continue;
        }

        if current_line.starts_with("fn ") {
            let function_header = current_line;
            let (function_name, parameters) = parse_function_header(function_header)?;

            if !function_header.contains('{') {
                while line_index < source_lines.len()
                    && source_lines[line_index].trim().is_empty()
                {
                    line_index += 1;
                }

                if line_index >= source_lines.len()
                    || source_lines[line_index].trim() != "{"
                {
                    return Err("Expected '{' after function header".into());
                }
                line_index += 1;
            }

            let mut function_body = String::new();
            let mut brace_depth = 1;

            while line_index < source_lines.len() {
                let body_line = source_lines[line_index];
                line_index += 1;

                if body_line.trim() == "{" {
                    brace_depth += 1;
                    continue;
                }
                if body_line.trim() == "}" {
                    brace_depth -= 1;
                    if brace_depth == 0 {
                        break;
                    }
                    continue;
                }

                function_body.push_str(body_line);
                function_body.push('\n');
            }

            let body_statements = parse_kas_program(&function_body)?;
            parsed_statements.push(Statement::FunctionDeclaration(
                function_name,
                parameters,
                body_statements,
            ));
            continue;
        }

        if current_line.starts_with("if ") {
            let condition_part = current_line.strip_prefix("if ").unwrap();
            let condition_expr = if let Some(cond) = condition_part.strip_suffix('{') {
                cond.trim()
            } else {
                condition_part.trim()
            };

            let condition = parse_expression(condition_expr)?;

            if !current_line.contains('{') {
                while line_index < source_lines.len()
                    && source_lines[line_index].trim().is_empty()
                {
                    line_index += 1;
                }

                if line_index >= source_lines.len()
                    || source_lines[line_index].trim() != "{"
                {
                    return Err("Expected '{' after if condition".into());
                }
                line_index += 1;
            }

            let mut then_body = String::new();
            let mut brace_depth = 1;

            while line_index < source_lines.len() {
                let body_line = source_lines[line_index];

                if body_line.trim() == "{" {
                    brace_depth += 1;
                    line_index += 1;
                    continue;
                }
                if body_line.trim() == "}" {
                    brace_depth -= 1;
                    if brace_depth == 0 {
                        line_index += 1;
                        break;
                    }
                    line_index += 1;
                    continue;
                }

                then_body.push_str(body_line);
                then_body.push('\n');
                line_index += 1;
            }

            let then_statements = parse_kas_program(&then_body)?;

            let mut else_statements = None;
            if line_index < source_lines.len() {
                let next_line = source_lines[line_index].trim();
                if next_line == "else" || next_line.starts_with("else ") {
                    line_index += 1;

                    while line_index < source_lines.len()
                        && source_lines[line_index].trim().is_empty()
                    {
                        line_index += 1;
                    }

                    if line_index >= source_lines.len()
                        || source_lines[line_index].trim() != "{"
                    {
                        return Err("Expected '{' after else".into());
                    }
                    line_index += 1;

                    let mut else_body = String::new();
                    let mut brace_depth = 1;

                    while line_index < source_lines.len() {
                        let body_line = source_lines[line_index];

                        if body_line.trim() == "{" {
                            brace_depth += 1;
                            line_index += 1;
                            continue;
                        }
                        if body_line.trim() == "}" {
                            brace_depth -= 1;
                            if brace_depth == 0 {
                                line_index += 1;
                                break;
                            }
                            line_index += 1;
                            continue;
                        }

                        else_body.push_str(body_line);
                        else_body.push('\n');
                        line_index += 1;
                    }

                    else_statements = Some(parse_kas_program(&else_body)?);
                }
            }

            parsed_statements.push(Statement::IfStatement(condition, then_statements, else_statements));
            continue;
        }

        if current_line.starts_with("while ") {
            let condition_part = current_line.strip_prefix("while ").unwrap();
            let condition_expr = if let Some(cond) = condition_part.strip_suffix('{') {
                cond.trim()
            } else {
                condition_part.trim()
            };

            let condition = parse_expression(condition_expr)?;

            if !current_line.contains('{') {
                while line_index < source_lines.len()
                    && source_lines[line_index].trim().is_empty()
                {
                    line_index += 1;
                }

                if line_index >= source_lines.len()
                    || source_lines[line_index].trim() != "{"
                {
                    return Err("Expected '{' after while condition".into());
                }
                line_index += 1;
            }

            let mut loop_body = String::new();
            let mut brace_depth = 1;

            while line_index < source_lines.len() {
                let body_line = source_lines[line_index];
                line_index += 1;

                if body_line.trim() == "{" {
                    brace_depth += 1;
                    continue;
                }
                if body_line.trim() == "}" {
                    brace_depth -= 1;
                    if brace_depth == 0 {
                        break;
                    }
                    continue;
                }

                loop_body.push_str(body_line);
                loop_body.push('\n');
            }

            let body_statements = parse_kas_program(&loop_body)?;
            parsed_statements.push(Statement::WhileLoop(condition, body_statements));
            continue;
        }

        if current_line.starts_with("for ") {
            let for_part = current_line.strip_prefix("for ").unwrap();
            let parts: Vec<&str> = for_part.split(" in ").collect();
            if parts.len() != 2 {
                return Err("Invalid for loop syntax. Expected: for var in iterable".into());
            }

            let iterator_var = parts[0].trim().to_string();
            let iterable_part = parts[1].trim();
            let iterable_expr = if let Some(expr) = iterable_part.strip_suffix('{') {
                expr.trim()
            } else {
                iterable_part
            };

            let iterable = parse_expression(iterable_expr)?;

            if !current_line.contains('{') {
                while line_index < source_lines.len()
                    && source_lines[line_index].trim().is_empty()
                {
                    line_index += 1;
                }

                if line_index >= source_lines.len()
                    || source_lines[line_index].trim() != "{"
                {
                    return Err("Expected '{' after for loop header".into());
                }
                line_index += 1;
            }

            let mut loop_body = String::new();
            let mut brace_depth = 1;

            while line_index < source_lines.len() {
                let body_line = source_lines[line_index];
                line_index += 1;

                if body_line.trim() == "{" {
                    brace_depth += 1;
                    continue;
                }
                if body_line.trim() == "}" {
                    brace_depth -= 1;
                    if brace_depth == 0 {
                        break;
                    }
                    continue;
                }

                loop_body.push_str(body_line);
                loop_body.push('\n');
            }

            let body_statements = parse_kas_program(&loop_body)?;
            parsed_statements.push(Statement::ForLoop(iterator_var, iterable, body_statements));
            continue;
        }

        if current_line == "break" {
            parsed_statements.push(Statement::BreakStatement);
            continue;
        }

        if current_line == "continue" {
            parsed_statements.push(Statement::ContinueStatement);
            continue;
        }

        if let Some(exit_expr) = current_line.strip_prefix("exit ") {
            parsed_statements.push(Statement::ExitStatement(
                parse_expression(exit_expr.trim())?
            ));
            continue;
        }

        if let Some(assert_part) = current_line.strip_prefix("assert ") {
            if let Some((condition_str, message_str)) = assert_part.split_once(',') {
                let condition = parse_expression(condition_str.trim())?;
                let message = message_str.trim().trim_matches('"').to_string();
                parsed_statements.push(Statement::AssertStatement(condition, Some(message)));
            } else {
                let condition = parse_expression(assert_part.trim())?;
                parsed_statements.push(Statement::AssertStatement(condition, None));
            }
            continue;
        }

        if let Some(declaration_rest) = current_line.strip_prefix("let ") {
            let Some((variable_name, expression_text)) = declaration_rest.split_once('=') else {
                return Err("Variable declaration requires '=' operator".into());
            };
            parsed_statements.push(Statement::VariableDeclaration(
                variable_name.trim().into(),
                parse_expression(expression_text.trim())?,
            ));
            continue;
        }

        if is_assignment_statement(current_line) {
            if let Some((left_part, right_part)) = current_line.split_once('=') {
                let left = left_part.trim();
                let right = right_part.trim();
                
                if left.contains('[') && left.contains(']') {
                    if let Some((var_name, index_part)) = left.split_once('[') {
                        let index_str = index_part.trim_end_matches(']');
                        parsed_statements.push(Statement::IndexAssignment(
                            var_name.trim().to_string(),
                            parse_expression(index_str)?,
                            parse_expression(right)?,
                        ));
                        continue;
                    }
                }
                
                if !left.contains(' ') && !left.contains('(') {
                    parsed_statements.push(Statement::Assignment(
                        left.to_string(),
                        parse_expression(right)?,
                    ));
                    continue;
                }
            }
        }

        if let Some(print_expression) = current_line.strip_prefix("print ") {
            parsed_statements.push(Statement::PrintStatement(
                parse_expression(print_expression.trim())?
            ));
            continue;
        }

        if let Some(return_expression) = current_line.strip_prefix("return ") {
            parsed_statements.push(Statement::ReturnStatement(
                parse_expression(return_expression.trim())?
            ));
            continue;
        }

        if let Some(import_specification) = current_line.strip_prefix("import ") {
            parsed_statements.push(Statement::ImportStatement(
                import_specification.trim().to_string()
            ));
            continue;
        }

        parsed_statements.push(Statement::ExpressionStatement(
            parse_expression(current_line)?
        ));
    }

    Ok(parsed_statements)
}

fn is_assignment_statement(line: &str) -> bool {
    if !line.contains('=') {
        return false;
    }
    
    if line.contains("==") || line.contains("!=") || line.contains("<=") || line.contains(">=") {
        return false;
    }
    
    true
}

fn parse_function_header(header_text: &str) -> Result<(String, Vec<String>), String> {
    let mut processed_header = header_text.trim().to_string();

    if let Some((left_part, _)) = processed_header.split_once('{') {
        processed_header = left_part.trim().to_string();
    }

    let remaining_text = processed_header
        .strip_prefix("fn ")
        .ok_or("Function header must begin with 'fn '")?
        .trim();

    let Some((function_name, parameters_part)) = remaining_text.split_once('(') else {
        return Err("Function header missing opening parenthesis".into());
    };

    let function_name = function_name.trim().to_string();

    let Some((parameters_text, _)) = parameters_part.split_once(')') else {
        return Err("Function header missing closing parenthesis".into());
    };

    let parameters_text = parameters_text.trim();
    let mut parameter_list = vec![];

    if !parameters_text.is_empty() {
        for parameter in parameters_text.split(',') {
            parameter_list.push(parameter.trim().to_string());
        }
    }

    Ok((function_name, parameter_list))
}

fn tokenize_expression(input: &str) -> Result<Vec<String>, String> {
    let mut tokens = Vec::new();
    let mut character_index = 0usize;
    let input_bytes = input.as_bytes();

    while character_index < input_bytes.len() {
        let current_char = input_bytes[character_index] as char;

        if current_char.is_whitespace() {
            character_index += 1;
            continue;
        }

        if current_char == '"' {
            character_index += 1;
            let mut string_content = String::new();

            while character_index < input_bytes.len() {
                let string_char = input_bytes[character_index] as char;

                if string_char == '"' {
                    character_index += 1;
                    break;
                }

                if string_char == '\\' && character_index + 1 < input_bytes.len() {
                    let escape_char = input_bytes[character_index + 1] as char;
                    match escape_char {
                        'n' => string_content.push('\n'),
                        't' => string_content.push('\t'),
                        'r' => string_content.push('\r'),
                        '"' => string_content.push('"'),
                        '\\' => string_content.push('\\'),
                        _ => string_content.push(escape_char),
                    }
                    character_index += 2;
                    continue;
                }

                string_content.push(string_char);
                character_index += 1;
            }

            tokens.push(format!("\"{}\"", string_content));
            continue;
        }

        if character_index + 2 < input_bytes.len() {
            let three_char = format!("{}{}{}", 
                current_char, 
                input_bytes[character_index + 1] as char, 
                input_bytes[character_index + 2] as char
            );
            if three_char == "**=" {
                tokens.push("**=".to_string());
                character_index += 3;
                continue;
            }
        }

        if character_index + 1 < input_bytes.len() {
            let two_char = format!("{}{}", current_char, input_bytes[character_index + 1] as char);
            if ["==", "!=", "<=", ">=", "&&", "||", "**", "+=", "-=", "*=", "/=", "%=", "++", "--"].contains(&two_char.as_str()) {
                tokens.push(two_char);
                character_index += 2;
                continue;
            }
        }

        if "+-*/%()[],.=<>!&|?:;{}^".contains(current_char) {
            tokens.push(current_char.to_string());
            character_index += 1;
            continue;
        }

        let token_start = character_index;
        while character_index < input_bytes.len() {
            let ch = input_bytes[character_index] as char;
            if ch.is_whitespace() || "+-*/%()[],.=<>!&|?:;{}^".contains(ch) {
                break;
            }
            character_index += 1;
        }

        let token = String::from_utf8(input_bytes[token_start..character_index].to_vec())
            .map_err(|_| "Invalid UTF-8 in expression")?;
        
        if token == "true" || token == "false" || token == "null" {
            tokens.push(token);
        } else if token.contains('.') && token.parse::<f64>().is_ok() {
            tokens.push(token);
        } else {
            tokens.push(token);
        }
    }

    Ok(tokens)
}

pub fn parse_expression(input: &str) -> Result<Expression, String> {
    let tokens = tokenize_expression(input)?;
    let mut token_stream = TokenStream::new(tokens);
    parse_ternary_expression(&mut token_stream)
}

fn parse_ternary_expression(stream: &mut TokenStream) -> Result<Expression, String> {
    let condition = parse_logical_or_expression(stream)?;
    
    if stream.consume("?") {
        let true_expr = parse_logical_or_expression(stream)?;
        if !stream.consume(":") {
            return Err("Expected ':' in ternary expression".into());
        }
        let false_expr = parse_ternary_expression(stream)?;
        return Ok(Expression::TernaryOperation(
            Box::new(condition),
            Box::new(true_expr),
            Box::new(false_expr),
        ));
    }
    
    Ok(condition)
}

fn parse_logical_or_expression(stream: &mut TokenStream) -> Result<Expression, String> {
    let mut left_expression = parse_logical_and_expression(stream)?;

    while stream.peek() == Some("||") {
        stream.advance();
        let right_expression = parse_logical_and_expression(stream)?;
        left_expression = Expression::LogicalOperation(
            Box::new(left_expression),
            LogicalOp::Or,
            Box::new(right_expression),
        );
    }

    Ok(left_expression)
}

fn parse_logical_and_expression(stream: &mut TokenStream) -> Result<Expression, String> {
    let mut left_expression = parse_comparison_expression(stream)?;

    while stream.peek() == Some("&&") {
        stream.advance();
        let right_expression = parse_comparison_expression(stream)?;
        left_expression = Expression::LogicalOperation(
            Box::new(left_expression),
            LogicalOp::And,
            Box::new(right_expression),
        );
    }

    Ok(left_expression)
}

fn parse_comparison_expression(stream: &mut TokenStream) -> Result<Expression, String> {
    let mut left_expression = parse_additive_expression(stream)?;

    if let Some(op_str) = stream.peek() {
        let comparison_op = match op_str {
            "==" => Some(ComparisonOp::Equal),
            "!=" => Some(ComparisonOp::NotEqual),
            "<" => Some(ComparisonOp::LessThan),
            "<=" => Some(ComparisonOp::LessThanOrEqual),
            ">" => Some(ComparisonOp::GreaterThan),
            ">=" => Some(ComparisonOp::GreaterThanOrEqual),
            _ => None,
        };

        if let Some(op) = comparison_op {
            stream.advance();
            let right_expression = parse_additive_expression(stream)?;
            left_expression = Expression::ComparisonOperation(
                Box::new(left_expression),
                op,
                Box::new(right_expression),
            );
        }
    }

    Ok(left_expression)
}

fn parse_additive_expression(stream: &mut TokenStream) -> Result<Expression, String> {
    let mut left_expression = parse_multiplicative_expression(stream)?;

    loop {
        match stream.peek() {
            Some("+") => {
                stream.advance();
                let right_expression = parse_multiplicative_expression(stream)?;
                left_expression = Expression::BinaryOperation(
                    Box::new(left_expression),
                    BinaryOp::Add,
                    Box::new(right_expression),
                );
            }
            Some("-") => {
                stream.advance();
                let right_expression = parse_multiplicative_expression(stream)?;
                left_expression = Expression::BinaryOperation(
                    Box::new(left_expression),
                    BinaryOp::Subtract,
                    Box::new(right_expression),
                );
            }
            _ => break,
        }
    }

    Ok(left_expression)
}

fn parse_multiplicative_expression(stream: &mut TokenStream) -> Result<Expression, String> {
    let mut left_expression = parse_power_expression(stream)?;

    loop {
        match stream.peek() {
            Some("*") => {
                stream.advance();
                let right_expression = parse_power_expression(stream)?;
                left_expression = Expression::BinaryOperation(
                    Box::new(left_expression),
                    BinaryOp::Multiply,
                    Box::new(right_expression),
                );
            }
            Some("/") => {
                stream.advance();
                let right_expression = parse_power_expression(stream)?;
                left_expression = Expression::BinaryOperation(
                    Box::new(left_expression),
                    BinaryOp::Divide,
                    Box::new(right_expression),
                );
            }
            Some("%") => {
                stream.advance();
                let right_expression = parse_power_expression(stream)?;
                left_expression = Expression::BinaryOperation(
                    Box::new(left_expression),
                    BinaryOp::Modulo,
                    Box::new(right_expression),
                );
            }
            _ => break,
        }
    }

    Ok(left_expression)
}

fn parse_power_expression(stream: &mut TokenStream) -> Result<Expression, String> {
    let mut left_expression = parse_unary_expression(stream)?;

    while stream.peek() == Some("**") {
        stream.advance();
        let right_expression = parse_unary_expression(stream)?;
        left_expression = Expression::BinaryOperation(
            Box::new(left_expression),
            BinaryOp::Power,
            Box::new(right_expression),
        );
    }

    Ok(left_expression)
}

fn parse_unary_expression(stream: &mut TokenStream) -> Result<Expression, String> {
    match stream.peek() {
        Some("-") => {
            stream.advance();
            let operand = parse_unary_expression(stream)?;
            Ok(Expression::UnaryOperation(UnaryOp::Negate, Box::new(operand)))
        }
        Some("!") => {
            stream.advance();
            let operand = parse_unary_expression(stream)?;
            Ok(Expression::UnaryOperation(UnaryOp::Not, Box::new(operand)))
        }
        _ => parse_postfix_expression(stream),
    }
}

fn parse_postfix_expression(stream: &mut TokenStream) -> Result<Expression, String> {
    let mut expression = parse_primary_expression(stream)?;

    loop {
        if stream.consume(".") {
            let Some(member_name) = stream.advance() else {
                return Err("Expected member name after '.' operator".into());
            };
            expression = Expression::MemberAccess(Box::new(expression), member_name);
            continue;
        }

        if stream.consume("[") {
            let index_expr = parse_ternary_expression(stream)?;
            if !stream.consume("]") {
                return Err("Expected ']' after array index".into());
            }
            expression = Expression::IndexAccess(Box::new(expression), Box::new(index_expr));
            continue;
        }

        if stream.consume("(") {
            let mut arguments = vec![];

            if !stream.consume(")") {
                loop {
                    let argument = parse_ternary_expression(stream)?;
                    arguments.push(argument);

                    if stream.consume(")") {
                        break;
                    }

                    if !stream.consume(",") {
                        return Err("Expected ',' or ')' in function call".into());
                    }
                }
            }

            expression = Expression::FunctionCall(Box::new(expression), arguments);
            continue;
        }

        break;
    }

    Ok(expression)
}

fn parse_primary_expression(stream: &mut TokenStream) -> Result<Expression, String> {
    match stream.advance() {
        Some(token) if token == "(" => {
            let inner_expression = parse_ternary_expression(stream)?;
            if !stream.consume(")") {
                return Err("Expected closing parenthesis".into());
            }
            Ok(inner_expression)
        }
        Some(token) if token == "[" => {
            let mut elements = vec![];
            
            if !stream.consume("]") {
                loop {
                    let element = parse_ternary_expression(stream)?;
                    elements.push(element);
                    
                    if stream.consume("]") {
                        break;
                    }
                    
                    if !stream.consume(",") {
                        return Err("Expected ',' or ']' in array literal".into());
                    }
                }
            }
            
            Ok(Expression::ArrayLiteral(elements))
        }
        Some(token) if token.starts_with('"') && token.ends_with('"') => {
            Ok(Expression::StringLiteral(token.trim_matches('"').to_string()))
        }
        Some(token) if token == "true" => Ok(Expression::BooleanLiteral(true)),
        Some(token) if token == "false" => Ok(Expression::BooleanLiteral(false)),
        Some(token) if token == "null" => Ok(Expression::Variable("null".to_string())),
        Some(token) => {
            if let Ok(float_value) = token.parse::<f64>() {
                if token.contains('.') {
                    Ok(Expression::FloatLiteral(float_value))
                } else {
                    Ok(Expression::IntegerLiteral(float_value as i64))
                }
            } else if let Ok(numeric_value) = token.parse::<i64>() {
                Ok(Expression::IntegerLiteral(numeric_value))
            } else {
                Ok(Expression::Variable(token))
            }
        }
        None => Err("Unexpected end of expression".into()),
    }
}