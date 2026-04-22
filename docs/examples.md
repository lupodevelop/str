# Examples

Copy-pasteable integration snippets. For full API see [api_reference.md](api_reference.md).

---

## Grapheme-aware search

```gleam
import str

pub fn search_examples() {
  // Emoji is ONE grapheme cluster — index counts clusters, not bytes
  let idx = str.index_of("Hello 👨‍👩‍👧‍👦 World", "World")
  // Ok(8)

  let last = str.last_index_of("hello hello hello", "hello")
  // Ok(12)

  let has_any = str.contains_any("hello world", ["foo", "world"])
  // True

  let has_all = str.contains_all("hello world", ["hello", "world"])
  // True
}
```

---

## KMP map caching (hot loops)

```gleam
import str
import str/advanced

pub fn cached_search() {
  let pattern = "abababab"
  let maps = advanced.build_kmp_maps(pattern)
  let pmap = maps.0
  let pimap = maps.1

  // Reuse pre-built maps across many texts — avoids rebuilding prefix table
  let idx1 = advanced.kmp_index_of_with_maps("first long text...", pattern, pmap, pimap)
  let all  = advanced.kmp_search_all_with_maps("another text...", pmap, pimap)
}
```

> Prefer `str/advanced` for explicit algorithm control. Use `index_of_auto`
> only for exploratory/non-critical paths.

---

## Grapheme length and boundaries

```gleam
import str

pub fn length_examples() {
  str.length("Hello")   // 5
  str.length("👨‍👩‍👧‍👦")   // 1 — family emoji is one grapheme cluster
  str.length("🇮🇹")      // 1 — flag sequence
  str.length("café")    // 4 — combining accent stays attached
}
```

---

## HTML escaping

```gleam
import str

let safe = str.escape_html("<script>alert('xss')</script>")
// "&lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;"

let original = str.unescape_html("&lt;div&gt;Hello&lt;/div&gt;")
// "<div>Hello</div>"
```

---

## Validation

```gleam
import str

str.is_uppercase("HELLO123")      // True
str.is_lowercase("hello_world")   // True
str.is_title_case("Hello World")  // True
str.is_title_case("hello World")  // False
str.is_ascii("hello!@#")          // True
str.is_ascii("café")              // False
str.is_hex("DEADBEEF")            // True
str.is_printable("hello\n")       // False
```

---

## Similarity and distance

```gleam
import str

str.distance("kitten", "sitting")   // 3
str.similarity("hello", "hallo")    // 0.8
str.hamming_distance("karolin", "kathrin")  // Ok(3)
```

---

## Take / drop from right

```gleam
import str

str.take_right("hello world", 3)  // "rld"
str.drop_right("file.txt", 4)     // "file"
str.take_right("Hello 👋🏽", 1)    // "👋🏽" — skin-tone modifier preserved
```

---

## Partition and split

```gleam
import str

str.partition("a-b-c", "-")           // #("a", "-", "b-c")
str.rpartition("a-b-c", "-")          // #("a-b", "-", "c")
str.splitn("one-two-three-four", "-", 2)  // ["one", "two-three-four"]
```

---

## Padding and fill

```gleam
import str

str.pad_left("42", 5, "0")          // "00042"
str.fill("x", 5, "-", str.Left)     // "----x"
str.fill("x", 5, "-", str.Right)    // "x----"
str.fill("x", 5, "-", str.Both)     // "--x--"
```

---

## String manipulation

```gleam
import str

str.replace_first("aaa", "a", "b")    // "baa"
str.replace_last("aaa", "a", "b")     // "aab"
str.normalize_whitespace("  hello   world  \n")  // "hello world"
str.reverse_words("hello beautiful world")        // "world beautiful hello"
str.initials("John Fitzgerald Kennedy")           // "JFK"
str.strip("..hello..", ".")                       // "hello"
str.squeeze("aabbcc", "b")                        // "aabcc"
```

---

## Slugification

```gleam
import str

str.slugify("Crème Brûlée — Recipe 2025!")
// → "creme-brulee-recipe-2025"

str.slugify_opts("one two three four", 2, "-", False)
// → "one-two"

// Preserve Unicode characters
str.slugify_opts("Héllo Wörld", -1, "-", True)
// → "héllo-wörld"
```

---

## Case conversions

```gleam
import str

str.to_snake_case("Hello World")    // → "hello_world"
str.to_camel_case("hello world")    // → "helloWorld"
str.to_pascal_case("hello world")   // → "HelloWorld"
str.to_kebab_case("Hello World")    // → "hello-world"
str.camel_to_snake("camelCase")     // → "camel_case"
str.snake_to_camel("snake_case")    // → "snakeCase"
```

---

## OTP-based Unicode normalization

See [otp_integration.md](otp_integration.md) for passing NFC/NFD normalizers
to `ascii_fold_with_normalizer` and `slugify_with_normalizer`.
