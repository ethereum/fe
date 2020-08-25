use yultsur::*;

/// Builds a Yul block that returns the runtime object.
pub fn runtime() -> yul::Code {
    let block = block! {
        (let size := datasize("runtime"))
        (datacopy(0, (dataoffset("runtime")), size))
        (return(0, size))
    };

    yul::Code { block }
}

#[test]
fn test_constructor() {
    assert_eq!(
        runtime().to_string(),
        r#"code { let size := datasize("runtime") datacopy(0, dataoffset("runtime"), size) return(0, size) }"#,
        "incorrect constructor"
    )
}
