#!/usr/bin/env python3
"""
Character Tables Documentation Generator

Generates machine-readable documentation (docs/character_tables.json) from
internal Gleam transliteration and decomposition tables.

This script parses:
- src/str/internal/generated_translit_pairs.gleam (character replacements)
- src/str/internal/generated_decompose_pairs.gleam (Latin decompositions)

Output format: JSON with "replacements" and "decompositions" arrays.

Usage:
    python3 scripts/generate_character_tables.py

The generated JSON file is used by documentation and can be consumed by
external tools that need to understand the library's character mappings.
"""
import json
import re
from pathlib import Path
from typing import List, Tuple

# Project structure
ROOT = Path(__file__).resolve().parents[1]
TRANSLIT = ROOT / "src" / "str" / "internal" / "generated_translit_pairs.gleam"
DECOMPOSE = ROOT / "src" / "str" / "internal" / "generated_decompose_pairs.gleam"
OUT = ROOT / "docs" / "character_tables.json"

# Regex patterns for parsing Gleam list syntax
PAIR_RE = re.compile(r'#\(\s*"([^"]+)"\s*,\s*"([^"]*)"\s*\)')
U_BRACE_RE = re.compile(r'\\u\{([0-9A-Fa-f]+)\}')

def extract_pairs(path: Path) -> List[Tuple[str, str]]:
    """
    Extract character mapping pairs from a Gleam source file.
    
    Searches for patterns like ["from", "to"] in the file content and
    normalizes Unicode escape sequences to JSON-compatible format.
    
    Args:
        path: Path to the Gleam source file
        
    Returns:
        List of (from, to) tuples representing character mappings
        
    Raises:
        FileNotFoundError: If the source file doesn't exist
    """
    if not path.exists():
        raise FileNotFoundError(f"Source file not found: {path}")
        
    text = path.read_text(encoding="utf-8")
    pairs = []
    
    for m in PAIR_RE.finditer(text):
        from_char, to_char = m.group(1), m.group(2)
        
        # Normalize Gleam \u{XXXX} syntax to JSON \uXXXX format
        to_normalized = U_BRACE_RE.sub(
            lambda mm: "\\u" + mm.group(1).zfill(4),
            to_char
        )
        
        pairs.append((from_char, to_normalized))
    
    return pairs


def main() -> None:
    """
    Main entry point for the generator.
    
    Reads source files, extracts character mappings, and writes JSON output.
    """
    print(f"Reading transliteration table from {TRANSLIT.relative_to(ROOT)}...")
    replacements = extract_pairs(TRANSLIT)
    print(f"  Found {len(replacements)} replacement mappings")
    
    print(f"Reading decomposition table from {DECOMPOSE.relative_to(ROOT)}...")
    decompositions = extract_pairs(DECOMPOSE)
    print(f"  Found {len(decompositions)} decomposition mappings")
    
    # Build output structure
    data = {
        "version": "1.0",
        "generator": "scripts/generate_character_tables.py",
        "replacements": [
            {"from": from_char, "to": to_char}
            for from_char, to_char in replacements
        ],
        "decompositions": [
            {"from": from_char, "to": to_char}
            for from_char, to_char in decompositions
        ],
    }
    
    # Ensure output directory exists
    OUT.parent.mkdir(parents=True, exist_ok=True)
    
    # Write formatted JSON (preserve Unicode characters, readable indentation)
    OUT.write_text(
        json.dumps(data, ensure_ascii=False, indent=2, sort_keys=True),
        encoding="utf-8"
    )
    
    print(f"\n✓ Successfully wrote {OUT.relative_to(ROOT)}")
    print(f"  Total mappings: {len(replacements) + len(decompositions)}")


if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print(f"Error: {e}")
        exit(1)
