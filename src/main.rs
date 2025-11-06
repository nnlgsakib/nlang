use clap::Parser;
use std::path::PathBuf;
use nlang::cli;

#[derive(Parser)]
#[command(name = "nlang")]
#[command(about = "A new programming language with Python-like syntax compiled to machine code using C")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(clap::Subcommand)]
enum Commands {
    /// Compile an Nlang file to machine code
    #[command(alias = "c")]
    Compile {
        /// Input file to compile
        input: PathBuf,
        
        /// Output file name (optional)
        #[arg(short, long)]
        output: Option<PathBuf>,
        
        /// Generate lexer tokens JSON output
        #[arg(long)]
        lex: bool,
        
        /// Generate AST JSON output
        #[arg(long)]
        gen_ast: bool,
    },
    
    /// Run an Nlang file directly
    #[command(alias = "r")]
    Run {
        /// Input file to run
        input: PathBuf,
    },
    
    /// Generate C code from an Nlang file
    #[command(alias = "c-gen")]
    GenerateC {
        /// Input file to generate C code from
        input: PathBuf,
        
        /// Output C file name (optional)
        #[arg(short, long)]
        output: Option<PathBuf>,
        
        /// Generate lexer tokens JSON output
        #[arg(long)]
        lex: bool,
        
        /// Generate AST JSON output
        #[arg(long)]
        gen_ast: bool,
    },
    
    /// Generate lexer tokens from an Nlang file
    #[command(alias = "l")]
    Lex {
        /// Input file to generate tokens from
        input: PathBuf,
        
        /// Output JSON file name (optional)
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
    
    /// Generate AST JSON from an Nlang file
    #[command(alias = "ast")]
    GenAst {
        /// Input file to generate AST from
        input: PathBuf,
        
        /// Output JSON file name (optional)
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Compile { input, output, lex, gen_ast } => {
            cli::compile(input, output, lex, gen_ast)?;
        }
        Commands::Run { input } => {
            cli::run(input)?;
        }
        Commands::GenerateC { input, output, lex, gen_ast } => {
            cli::generate_c(input, output, lex, gen_ast)?;
        }
        Commands::Lex { input, output } => {
            cli::lex(input, output)?;
        }
        Commands::GenAst { input, output } => {
            cli::gen_ast(input, output)?;
        }
    }

    Ok(())
}
