#!/usr/bin/env python3
"""
Generate comprehensive transliteration tables for both Gleam and Erlang FFI.

This script creates:
1. src/str/internal/generated_translit_pairs.gleam - Pure Gleam implementation
2. src/str_ffi.erl - Erlang FFI with native Unicode support

The tables include comprehensive Unicode to ASCII mappings for common accented
characters, ligatures, and symbols from Latin, Greek, Cyrillic and other scripts.

Usage:
    python3 scripts/generate_comprehensive_translit.py
"""

# Comprehensive Unicode -> ASCII transliteration table
# Based on common standards including CLDR, Unidecode, and web best practices
TRANSLIT_PAIRS = [
    # Latin-1 Supplement (U+00C0 - U+00FF)
    ("À", "A"), ("Á", "A"), ("Â", "A"), ("Ã", "A"), ("Ä", "A"), ("Å", "A"),
    ("à", "a"), ("á", "a"), ("â", "a"), ("ã", "a"), ("ä", "a"), ("å", "a"),
    ("Ç", "C"), ("ç", "c"),
    ("È", "E"), ("É", "E"), ("Ê", "E"), ("Ë", "E"),
    ("è", "e"), ("é", "e"), ("ê", "e"), ("ë", "e"),
    ("Ì", "I"), ("Í", "I"), ("Î", "I"), ("Ï", "I"),
    ("ì", "i"), ("í", "i"), ("î", "i"), ("ï", "i"),
    ("Ð", "D"), ("ð", "d"),
    ("Ñ", "N"), ("ñ", "n"),
    ("Ò", "O"), ("Ó", "O"), ("Ô", "O"), ("Õ", "O"), ("Ö", "O"), ("Ø", "O"),
    ("ò", "o"), ("ó", "o"), ("ô", "o"), ("õ", "o"), ("ö", "o"), ("ø", "o"),
    ("Ù", "U"), ("Ú", "U"), ("Û", "U"), ("Ü", "U"),
    ("ù", "u"), ("ú", "u"), ("û", "u"), ("ü", "u"),
    ("Ý", "Y"), ("ý", "y"), ("ÿ", "y"),
    ("Þ", "TH"), ("þ", "th"),
    ("ß", "ss"),
    ("Æ", "AE"), ("æ", "ae"),
    ("Œ", "OE"), ("œ", "oe"),
    
    # Latin Extended-A (U+0100 - U+017F)
    ("Ā", "A"), ("ā", "a"), ("Ă", "A"), ("ă", "a"), ("Ą", "A"), ("ą", "a"),
    ("Ć", "C"), ("ć", "c"), ("Ĉ", "C"), ("ĉ", "c"), ("Ċ", "C"), ("ċ", "c"), ("Č", "C"), ("č", "c"),
    ("Ď", "D"), ("ď", "d"), ("Đ", "D"), ("đ", "d"),
    ("Ē", "E"), ("ē", "e"), ("Ĕ", "E"), ("ĕ", "e"), ("Ė", "E"), ("ė", "e"),
    ("Ę", "E"), ("ę", "e"), ("Ě", "E"), ("ě", "e"),
    ("Ĝ", "G"), ("ĝ", "g"), ("Ğ", "G"), ("ğ", "g"), ("Ġ", "G"), ("ġ", "g"),
    ("Ģ", "G"), ("ģ", "g"),
    ("Ĥ", "H"), ("ĥ", "h"), ("Ħ", "H"), ("ħ", "h"),
    ("Ĩ", "I"), ("ĩ", "i"), ("Ī", "I"), ("ī", "i"), ("Ĭ", "I"), ("ĭ", "i"),
    ("Į", "I"), ("į", "i"), ("İ", "I"), ("ı", "i"),
    ("Ĵ", "J"), ("ĵ", "j"),
    ("Ķ", "K"), ("ķ", "k"),
    ("Ĺ", "L"), ("ĺ", "l"), ("Ļ", "L"), ("ļ", "l"), ("Ľ", "L"), ("ľ", "l"),
    ("Ŀ", "L"), ("ŀ", "l"), ("Ł", "L"), ("ł", "l"),
    ("Ń", "N"), ("ń", "n"), ("Ņ", "N"), ("ņ", "n"), ("Ň", "N"), ("ň", "n"),
    ("Ō", "O"), ("ō", "o"), ("Ŏ", "O"), ("ŏ", "o"), ("Ő", "O"), ("ő", "o"),
    ("Ŕ", "R"), ("ŕ", "r"), ("Ŗ", "R"), ("ŗ", "r"), ("Ř", "R"), ("ř", "r"),
    ("Ś", "S"), ("ś", "s"), ("Ŝ", "S"), ("ŝ", "s"), ("Ş", "S"), ("ş", "s"),
    ("Š", "S"), ("š", "s"),
    ("Ţ", "T"), ("ţ", "t"), ("Ť", "T"), ("ť", "t"), ("Ŧ", "T"), ("ŧ", "t"),
    ("Ũ", "U"), ("ũ", "u"), ("Ū", "U"), ("ū", "u"), ("Ŭ", "U"), ("ŭ", "u"),
    ("Ů", "U"), ("ů", "u"), ("Ű", "U"), ("ű", "u"), ("Ų", "U"), ("ų", "u"),
    ("Ŵ", "W"), ("ŵ", "w"),
    ("Ŷ", "Y"), ("ŷ", "y"), ("Ÿ", "Y"),
    ("Ź", "Z"), ("ź", "z"), ("Ż", "Z"), ("ż", "z"), ("Ž", "Z"), ("ž", "z"),
    
    # Latin Extended-B (U+0180 - U+024F) - selection
    ("Ș", "S"), ("ș", "s"), ("Ț", "T"), ("ț", "t"),
    
    # Currency symbols
    ("€", "EUR"), ("£", "GBP"), ("¥", "YEN"), ("¢", "cent"),
    ("¤", "currency"), ("₹", "INR"), ("₽", "RUB"), ("₴", "UAH"),
    
    # Greek (U+0370 - U+03FF) - common letters
    ("Α", "A"), ("α", "a"), ("Β", "B"), ("β", "b"), ("Γ", "G"), ("γ", "g"),
    ("Δ", "D"), ("δ", "d"), ("Ε", "E"), ("ε", "e"), ("Ζ", "Z"), ("ζ", "z"),
    ("Η", "H"), ("η", "h"), ("Θ", "TH"), ("θ", "th"), ("Ι", "I"), ("ι", "i"),
    ("Κ", "K"), ("κ", "k"), ("Λ", "L"), ("λ", "l"), ("Μ", "M"), ("μ", "m"),
    ("Ν", "N"), ("ν", "n"), ("Ξ", "KS"), ("ξ", "ks"), ("Ο", "O"), ("ο", "o"),
    ("Π", "P"), ("π", "p"), ("Ρ", "R"), ("ρ", "r"), ("Σ", "S"), ("σ", "s"),
    ("ς", "s"), ("Τ", "T"), ("τ", "t"), ("Υ", "Y"), ("υ", "y"),
    ("Φ", "F"), ("φ", "f"), ("Χ", "CH"), ("χ", "ch"), ("Ψ", "PS"), ("ψ", "ps"),
    ("Ω", "O"), ("ω", "o"),
    
    # Cyrillic (U+0400 - U+04FF) - common letters
    ("А", "A"), ("а", "a"), ("Б", "B"), ("б", "b"), ("В", "V"), ("в", "v"),
    ("Г", "G"), ("г", "g"), ("Д", "D"), ("д", "d"), ("Е", "E"), ("е", "e"),
    ("Ё", "Yo"), ("ё", "yo"), ("Ж", "Zh"), ("ж", "zh"), ("З", "Z"), ("з", "z"),
    ("И", "I"), ("и", "i"), ("Й", "Y"), ("й", "y"), ("К", "K"), ("к", "k"),
    ("Л", "L"), ("л", "l"), ("М", "M"), ("м", "m"), ("Н", "N"), ("н", "n"),
    ("О", "O"), ("о", "o"), ("П", "P"), ("п", "p"), ("Р", "R"), ("р", "r"),
    ("С", "S"), ("с", "s"), ("Т", "T"), ("т", "t"), ("У", "U"), ("у", "u"),
    ("Ф", "F"), ("ф", "f"), ("Х", "Kh"), ("х", "kh"), ("Ц", "Ts"), ("ц", "ts"),
    ("Ч", "Ch"), ("ч", "ch"), ("Ш", "Sh"), ("ш", "sh"), ("Щ", "Shch"), ("щ", "shch"),
    ("Ъ", ""), ("ъ", ""), ("Ы", "Y"), ("ы", "y"), ("Ь", ""), ("ь", ""),
    ("Э", "E"), ("э", "e"), ("Ю", "Yu"), ("ю", "yu"), ("Я", "Ya"), ("я", "ya"),
    
    # Mathematical symbols
    ("×", "x"), ("÷", "/"), ("±", "+/-"),
    ("≤", "<="), ("≥", ">="),
    
    # Dashes (convert to ASCII hyphen)
    ("–", "-"), ("—", "-"), ("‐", "-"), ("‑", "-"),
    
    # Other common symbols
    ("©", "(c)"), ("®", "(r)"), ("°", "deg"),
    ("•", "*"), ("·", "."),
]


def generate_gleam_module():
    """Generate Gleam module with transliteration pairs."""
    lines = ["// Generated by scripts/generate_comprehensive_translit.py", ""]
    lines.append("pub fn replacements_generated() -> List(#(String, String)) {")
    lines.append("  [")
    for char, replacement in TRANSLIT_PAIRS:
        # Escape double quotes and backslashes for Gleam strings
        char_escaped = char.replace("\\", "\\\\").replace('"', '\\"')
        replacement_escaped = replacement.replace("\\", "\\\\").replace('"', '\\"')
        lines.append(f'    #("{char_escaped}", "{replacement_escaped}"),')
    lines.append("  ]")
    lines.append("}")
    return "\n".join(lines) + "\n"


def generate_erlang_ffi():
    """Generate Erlang FFI module with transliteration table."""
    lines = [
        "-module(str_ffi).",
        "-export([decompose_nfd/1, remove_combining_marks/1, translit_replacements/0]).",
        "",
        "%% @doc Decompose a string into NFD (Canonical Decomposition) form.",
        "%% Converts characters like 'é' into base + combining mark ('e' + '´').",
        "decompose_nfd(Str) when is_binary(Str) ->",
        "    unicode:characters_to_nfd_binary(Str).",
        "",
        "%% @doc Remove combining marks from a string.",
        "%% Filters out code points in all Unicode combining mark ranges:",
        "%%   U+0300-U+036F  Combining Diacritical Marks",
        "%%   U+1AB0-U+1AFF  Combining Diacritical Marks Extended",
        "%%   U+1DC0-U+1DFF  Combining Diacritical Marks Supplement",
        "%%   U+20D0-U+20FF  Combining Diacritical Marks for Symbols",
        "%%   U+FE20-U+FE2F  Combining Half Marks",
        "remove_combining_marks(Str) when is_binary(Str) ->",
        "    unicode:characters_to_binary(",
        "        [CP || CP <- unicode:characters_to_list(Str),",
        "               not is_combining_mark(CP)]",
        "    ).",
        "",
        "is_combining_mark(CP) when CP >= 16#0300, CP =< 16#036F -> true;",
        "is_combining_mark(CP) when CP >= 16#1AB0, CP =< 16#1AFF -> true;",
        "is_combining_mark(CP) when CP >= 16#1DC0, CP =< 16#1DFF -> true;",
        "is_combining_mark(CP) when CP >= 16#20D0, CP =< 16#20FF -> true;",
        "is_combining_mark(CP) when CP >= 16#FE20, CP =< 16#FE2F -> true;",
        "is_combining_mark(_) -> false.",
        "",
        "%% @doc Return the comprehensive transliteration replacement table.",
        "%% Returns a list of {From, To} tuples for character replacement.",
        "translit_replacements() ->",
        "    [",
    ]
    
    for char, replacement in TRANSLIT_PAIRS:
        # Escape special Erlang characters
        char_escaped = char.replace("\\", "\\\\").replace('"', '\\"')
        replacement_escaped = replacement.replace("\\", "\\\\").replace('"', '\\"')
        lines.append(f'        {{<<"{char_escaped}"/utf8>>, <<"{replacement_escaped}">>}},')
    
    # Remove trailing comma from last entry
    if lines[-1].endswith(","):
        lines[-1] = lines[-1][:-1]
    lines.append("    ].")
    
    return "\n".join(lines) + "\n"


def main():
    from pathlib import Path
    
    root = Path(__file__).resolve().parents[1]
    
    # Generate Gleam module
    gleam_path = root / "src" / "str" / "internal" / "generated_translit_pairs.gleam"
    gleam_content = generate_gleam_module()
    gleam_path.write_text(gleam_content, encoding="utf-8")
    print(f"✓ Generated {gleam_path.relative_to(root)}")
    print(f"  Total mappings: {len(TRANSLIT_PAIRS)}")
    
    # Generate Erlang FFI
    erlang_path = root / "src" / "str_ffi.erl"
    erlang_content = generate_erlang_ffi()
    erlang_path.write_text(erlang_content, encoding="utf-8")
    print(f"✓ Generated {erlang_path.relative_to(root)}")
    print(f"  Total mappings: {len(TRANSLIT_PAIRS)}")
    
    print(f"\nSuccessfully generated comprehensive transliteration tables")
    print(f"Both Pure Gleam and Erlang FFI now have {len(TRANSLIT_PAIRS)} mappings")


if __name__ == "__main__":
    main()
