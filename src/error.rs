use ahash::AHashMap;
use colored::Colorize;
use lyneate::{Report, Theme, ThemeChars};

use crate::{
    source::{CodeArea, CodeSpan},
    util::hsv_to_rgb,
};

#[derive(Debug, Clone, Copy)]
pub struct RainbowColorGenerator {
    h: f64,
    s: f64,
    v: f64,
    hue_shift: f64,
}

impl RainbowColorGenerator {
    pub fn new(h: f64, s: f64, v: f64, hue_shift: f64) -> Self {
        Self { h, s, v, hue_shift }
    }

    pub fn next(&mut self) -> (u8, u8, u8) {
        let h0 = self.h / 360.0;

        self.h = (self.h + self.hue_shift).rem_euclid(360.0);

        hsv_to_rgb(h0, self.s, self.v)
    }
}

pub struct ErrorReport {
    pub typ: &'static str,
    pub msg: &'static str,

    pub labels: Vec<(CodeArea, String)>,
}

impl ErrorReport {
    pub fn display(self) {
        println!(
            "\n{}: {}",
            self.typ.truecolor(255, 72, 72).bold(),
            // ":".bright_red().bold(),
            self.msg
        );
        let mut src_map = AHashMap::default();

        for (area, msg) in self.labels {
            src_map
                .entry(area.src.name())
                .or_insert_with(|| (area.src.read().unwrap(), vec![]))
                .1
                .push((area.span, msg));
        }

        let mut colors = RainbowColorGenerator::new(345.0, 0.75, 1.0, 45.0);
        let theme = Theme {
            chars: ThemeChars {
                side_vertical_dotted: '·',
                ..Default::default()
            },
            ..Default::default()
        };

        for (src, (code, labels)) in src_map {
            println!("[{}]\n", src.truecolor(123, 184, 255));
            Report::new_byte_spanned(
                &code,
                labels.into_iter().map(|(span, msg)| {
                    (
                        span.into(),
                        msg.truecolor(150, 150, 150).to_string(),
                        colors.next(),
                    )
                }),
            )
            .with_theme(theme)
            .display();
            println!();
        }
    }
}

#[macro_export]
macro_rules! special_fmt {
    ($fmt:literal, $($arg:expr),* $(,)?) => {
        {
            use colored::Colorize;

            format!(
                $fmt,
                $(
                    format!("{}", $arg).bright_white(),
                )*
            )
        }
    };
}
