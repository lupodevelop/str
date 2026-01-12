/// Main test entry point for the str library.
/// 
/// This is the only file that should contain `pub fn main()` with `gleeunit.main()`.
/// All other test files in subdirectories are automatically discovered by gleeunit.
///
/// Test organization:
/// - test/unit/       - Unit tests for individual functions
/// - test/integration/ - Integration and parity tests
/// - test/fuzz/       - Deterministic property-based tests
///
/// Run with: gleam test
import gleeunit

pub fn main() -> Nil {
  gleeunit.main()
}

// Basic smoke test to verify test infrastructure works
pub fn hello_world_test() {
  let name = "Joe"
  let greeting = "Hello, " <> name <> "!"

  assert greeting == "Hello, Joe!"
}
