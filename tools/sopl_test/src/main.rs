use std::{fs::{self, read_to_string, File}, process::{Command, exit}, env::{set_current_dir, self, current_dir}, path::{PathBuf, Path}};
use std::io::Write;
fn unescape(stri: &str) -> String {
    let mut out = String::new();
    let mut chars = stri.chars().into_iter();
    while let Some(chr) = chars.next(){
        if chr == '\\' {
            let nc = &chars.next();
            assert!(nc.is_some(),"Error: could not unescape string, invalid string escape symbol");
            let nc = nc.unwrap();
            match nc {
                'n' => {
                    out.push_str("\n");
                }
                'r' => {
                    out.push_str("\r");
                }
                't' => {
                    out.push_str("\t");
                }
                _ => {
                    out.push(nc);
                }
            }
            
        }
        else {
            out.push(chr)
        }
    }
    out
}
fn remove_newlines(stri: &str) -> String {
    let mut out = String::new();
    let mut chars = stri.chars().into_iter();
    while let Some(chr) = chars.next() {
        if chr != '\n' && chr != '\r' {
            out.push(chr);
        }
    }
    out
}

struct ExpectedBehave {
    should_build_error: bool,
    args: Vec<String>,
    stdout: String,
    stdin: String,
    stderr: String,
}
impl ExpectedBehave {
    fn new() -> Self {
        Self { args: vec![], stdout: String::new(), stdin: String::new(), stderr: String::new(), should_build_error: false }
    }
    fn from_str(source: &str) -> Option<Self> {
        let mut out = Self::new();
        if source == "should_build_error" {
            out.should_build_error = true;
            return Some(out);
        }
        let loc = source.find("stdout:")? + "stdout:".len();
        let eloc = (&source[loc..]).find("end")? + loc;
        out.stdout = unescape(&remove_newlines(&source[loc..eloc]));
        let loc = source.find("args:")? + "args:".len();
        let eloc = (&source[loc..]).find("end")? + loc;
        out.args = {
            let mut o: Vec<String>=Vec::new();
            let v = unescape(&remove_newlines(&source[loc..eloc]));
            let i = v.split(' ');
            for str in i {
                if str != "" {
                    o.push(str.to_owned());
                }
            }
            o
        };
        
        let loc = source.find("stdin:")? + "stdin:".len();
        let eloc = (&source[loc..]).find("end")? + loc;
        out.stdin = unescape(&remove_newlines(&source[loc..eloc]));
        let loc = source.find("stderr:")? + "stderr:".len();
        let eloc = (&source[loc..]).find("end")? + loc;
        out.stderr = unescape(&remove_newlines(&source[loc..eloc]));
        Some(out)
    }
    fn to_file(&self, f: &mut File) -> std::io::Result<()>{
        writeln!(f,"args:\n{}end",self.args.join(" ").escape_default())?;
        writeln!(f,"stdout:\n{}end",self.stdout.escape_default())?;
        writeln!(f,"stderr:\n{}end",self.stderr.escape_default())?;
        writeln!(f,"stdin:\n{}end",self.stdin.escape_default())?;
        Ok(())
    }
}
const RED: &str = "\x1b[31;1m";
const GREEN: &str = "\x1b[32;1m";
const LIGHT_BLUE: &str= "\x1b[96;1m";
const RESET: &str = "\x1b[0m";
fn record(epath: &PathBuf, failed: &mut Vec<PathBuf>) {
    let sopl_exe: &str = "target/debug/sopl";
    
    let pname = Path::new(epath.file_name().unwrap()).with_extension("");
    let fname = pname.to_str().unwrap();
    println!("{LIGHT_BLUE}* Recording {}{RESET}",epath.to_string_lossy());  
    let compile_sopl = Command::new(sopl_exe).args(["-t", "nasm_x86_64", &format!("examples/{}.spl",fname), "-o", &format!("examples/int/{}.asm", fname), "-b"]).output().expect(&format!("Error: could not run build for {}",fname));
    if !compile_sopl.status.success() && compile_sopl.status.code().unwrap() != 101 {
        println!("{RED}Record failed for {}. Compiling exited with {}{RESET}",epath.to_string_lossy(),compile_sopl.status.code().unwrap_or(0));
        failed.push(epath.clone());
        return;
    }
    else {
        let outpath = format!("./tests/expected/{}.test",fname);
        //println!("Hello {}",outpath);
        let program_run = Command::new(format!("examples/int/{}",fname)).output();
        let mut out = ExpectedBehave::new();
        if program_run.is_err(){
            out.should_build_error = true;
        }
        else {
            let program_run = program_run.unwrap();
            out.stdout = String::from_utf8(program_run.stdout.clone()).unwrap();
            out.stderr = String::from_utf8(program_run.stderr.clone()).unwrap();
            
        }
        // let exp_str = read_to_string(format!("./tests/expected/{}.test",fname));
        // if exp_str.is_err() {
            //    println!("{RED}Record failed for {}. No expected case found at {}{RESET}",epath.to_string_lossy(),format!("./tests/expected/{}.test",fname));
            //    failed.push(epath);
            //    continue;
            // }
            let o = File::create(&outpath);
            if let Ok(mut o) = o {
                let res = out.to_file(&mut o);
                if res.is_ok() {
                    println!("{GREEN}Record succeeded for {}{RESET}\nWritting to: {}",fname,outpath);
                }
                else {
                    println!("{RED}Record failed for {}{RESET}\nCould not write to: {}\nReason: {}",fname,outpath,res.unwrap_err());    
                }
            }
            else {
                println!("{RED}Record failed for {}{RESET}\nCould not write to: {}\nReason: {}",fname,outpath,o.unwrap_err());
            }
    }
}
fn main() {
    let mut args: Vec<_> = env::args().collect();
    let sopl_exe: &str = "target/debug/sopl";
    args.remove(0);
    let sopl_home = "../../";
    if args.len() == 0 {
        let folder = "examples";
        //let sopl_back = current_dir().expect("Error: could not get current working directory");
        
        let mut failed: Vec<PathBuf> = Vec::new();
        println!("----------------");
        set_current_dir(sopl_home).expect("Error: could not change current working directory to sopl home!");
        println!("* cargo build");
        
        let command = Command::new("cargo").args(["build"]).output().expect("Could not run cargo build!");
        if !command.status.success() && command.status.code().unwrap() != 101 {
            println!("{RED}SOPL exited with {} {RESET}",command.status.code().unwrap_or(0));
        }
        println!("----------------");
        //set_current_dir(sopl_back).expect("Error: could not change current working directory to sopl back!");
        let data = read_to_string("./tests/config.cfg").expect("No config found");
        let ignored: Vec<String> = {
            let mut o: Vec<String> = Vec::new();
            for path in data.split("\n") {
                o.push(remove_newlines(path));
            }
            o
        };
        //println!("Ignored: {:?}",ignored);
        if let Ok(entries) = fs::read_dir(folder) {
            for entry in entries {
                if let Ok(entry) = entry {
                    let epath = entry.path();
                    if epath.is_file() && !ignored.contains(&epath.file_name().unwrap().to_str().unwrap().to_owned()){
                        let fname = epath.file_name().unwrap().to_str().unwrap();
                        let pname = Path::new(epath.file_name().unwrap()).with_extension("");
                        let fname = pname.to_str().unwrap();
                        println!("{LIGHT_BLUE}* Testing {} ({}){RESET}",epath.to_string_lossy(),fname);  
                        let compile_sopl = Command::new(sopl_exe).args(["-t","nasm_x86_64", &format!("examples/{}.spl",fname), "-o", &format!("examples/int/{}.asm", fname), "-b"]).output().expect(&format!("Error: could not run build for {}",fname));
                        if !compile_sopl.status.success() && compile_sopl.status.code().unwrap_or(0) != 101{
                            println!("{RED}Test failed for {}. Compiling exited with {}{RESET}\n{:?}",epath.to_string_lossy(),compile_sopl.status.code().unwrap_or(0),String::from_utf8_lossy(&compile_sopl.stderr));
                            failed.push(epath);
                            continue;
                        }
                        else {
                            
                            let exp_str = read_to_string(format!("./tests/expected/{}.test",fname));
                            if exp_str.is_err() {
                                println!("{RED}Test failed for {}. No expected case found at {}{RESET}",epath.to_string_lossy(),format!("./tests/expected/{}.test",fname));
                                failed.push(epath);
                                continue;
                            }
                            let exp_str = exp_str.unwrap();
                            let expected = ExpectedBehave::from_str(&exp_str);
                            if let Some(expected) = expected {
                                let program_run = Command::new(format!("examples/int/{}",fname)).args(&expected.args).output();
                                if program_run.is_err() && !expected.should_build_error {
                                    println!("{RED}Test failed for {} with args {:?}. Could not run test{RESET}",epath.to_string_lossy(),expected.args);
                                    failed.push(epath);
                                    continue;
                                }
                                else {
                                    let program_run = program_run.unwrap();
                                    if String::from_utf8(program_run.stderr.clone()).unwrap() != expected.stderr {
                                        println!("{RED}Test failed for {}. Stderr did not match:\nExpected: {}\nBut found: {}{RESET}",epath.to_string_lossy(),expected.stderr.to_owned().escape_default(),String::from_utf8(program_run.stderr).unwrap().escape_default());
                                        failed.push(epath)
                                    }
                                    else if String::from_utf8(program_run.stdout.clone()).unwrap() != expected.stdout {
                                        println!("{RED}Test failed for {}. Stdout did not match:\nExpected: {}\nBut found: {}{RESET}",epath.to_string_lossy(),expected.stdout.to_owned().escape_default(),String::from_utf8(program_run.stdout).unwrap().escape_default());
                                        failed.push(epath)
                                    }
                                    else {
                                        println!("{GREEN}Test succeeded for {}{RESET}",fname);
                                    }
                                }
                            }
                            else {
                                println!("Test failed for {}. Invalid test case syntax",epath.to_string_lossy());
                                failed.push(epath);
                                continue;
                            }
                        }
                    }
                    //println!("Hello {:?}",entry);
                    //entry.file_name()
                }
                else {
                    panic!("Entry: {} could not be found!",entry.unwrap_err());
                }
            }
        }
        else {
            panic!("Examples failed! Could not find folder {}",folder);
        }
        println!("-----------------------------");
        if failed.len() == 0 {
            println!("{GREEN}No issues were found{RESET}");
        }
        for fail in failed {
            println!("- {RED}Test failed for: {}{RESET}",fail.to_str().unwrap());
        }
    }
    else {
        let command = args.remove(0);
        match command.as_str() {
            "record" => {
                set_current_dir(sopl_home).expect("Error: could not change current working directory to sopl home!");
                let data = read_to_string("./tests/config.cfg").expect("No config found");
                let ignored: Vec<String> = {
                    let mut o: Vec<String> = Vec::new();
                    for path in data.split("\n") {
                        o.push(remove_newlines(path));
                    }
                    o
                };
                let option = args.remove(0);
                //let sopl_home = "../../";
                //set_current_dir(sopl_home).expect("Error: could not change current working directory to sopl home!");
                match option.as_str() {
                    "all" => {
                        let folder = "examples";
                        //let sopl_back = current_dir().expect("Error: could not get current working directory");
                        //let sopl_exe = "target/debug/sopl";
                        let mut failed: Vec<PathBuf> = Vec::new();
                        println!("----------------");
                        println!("* cargo build");
                        
                        let command = Command::new("cargo").args(["build"]).output().expect("Could not run cargo build!");
                        if !command.status.success() && command.status.code().unwrap_or(0) != 101{
                            eprintln!("SOPL exited with {}",command.status.code().unwrap_or(0));
                        }
                        println!("----------------");
                        //set_current_dir(sopl_back).expect("Error: could not change current working directory to sopl back!");
                    
                        if let Ok(entries) = fs::read_dir(folder) {
                            for entry in entries {
                                if let Ok(entry) = entry {
                                    let epath = entry.path();
                                    if epath.is_file() && !ignored.contains(&epath.file_name().unwrap().to_str().unwrap().to_owned()) {
                                        record(&epath,&mut failed);
                                    }
                                }
                                else {
                                    panic!("Entry: {} could not be found!",entry.unwrap_err());
                                }
                            }
                        }
                        else {
                            panic!("Examples failed! Could not find folder {} at {:?}",folder,current_dir());
                        }
                        println!("-----------------------------");
                        for fail in failed {
                            println!("- {RED}Test failed for: {}{RESET}",fail.to_str().unwrap());
                        }
                    }
                    _ => {
                        let p = format!("./examples/{}.spl",option);
                        //println!("p: {}",p);
                        let epath = Path::new(&p);
                        let mut failed: Vec<PathBuf> = Vec::new();
                        if epath.exists() && epath.is_file() {
                            record(&epath.to_path_buf(), &mut failed);
                        }
                        else {
                            println!("{RED}Unknown parameter {}{RESET}",option);
                        }
                    }
                }
            }
            _ => {
                println!("{RED}Unknown command {}{RESET}",command);
                exit(1);
            }
        }
    }
    //println!("Hello World!");
}