use colored::Colorize;
use itertools::Itertools;

use crate::source::{CodeArea, CodeSpan};

pub struct ErrorDisplay {
    pub typ: &'static str,
    pub msg: String,
    pub area: CodeArea,
}

impl ErrorDisplay {
    pub fn display(self) {
        println!(
            "\n{}: {}",
            self.typ.bright_red().bold(),
            self.msg.bright_yellow()
        );
        println!("{} {}", "in".dimmed(), self.area.src.name().bright_blue());

        let code = self.area.src.read();
        let lines = code.split_inclusive('\n').collect_vec();

        let calc_len = |n: usize| lines.iter().take(n).map(|l| l.len()).sum::<usize>();

        // println!("{:?}", lines);

        let mut pre_lines = 0;
        while calc_len(pre_lines) <= self.area.span.start {
            pre_lines += 1;
        }
        let mut line_count = 1;
        while calc_len(pre_lines + line_count - 1) < self.area.span.end {
            line_count += 1;
        }
        let new_start = calc_len(pre_lines - 1);
        let snippet = &code[new_start..calc_len(pre_lines + line_count - 1)].trim_end();

        let new_span = CodeSpan {
            start: self.area.span.start - new_start,
            end: self.area.span.end - new_start,
        };

        println!();
        for (l, s) in snippet.lines().enumerate() {
            println!(" {}    ", (pre_lines + l).to_string().dimmed())
        }

        println!(
            "\n{}{}{}",
            &snippet[..new_span.start].dimmed(),
            snippet[new_span.start..new_span.end]
                .bright_red()
                .underline(),
            snippet[new_span.end..].dimmed()
        );

        // println!(
        //     "hhh {}:{} {}:{} {:?}\n({})",
        //     pre_lines,
        //     calc_len(pre_lines - 1),
        //     line_count,
        //     calc_len(pre_lines + line_count - 1),
        //     new_span,
        //     snippet.underline()
        // );
    }
}
