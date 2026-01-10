import str/internal/generated_translit_pages as pages
import str/internal/bitarray_helpers as bits
import gleam/list

pub fn main() {
  check_all_pages()
}

pub fn check_all_pages() -> Nil {
  let offsets = pages.translit_pages_page_offsets
  let masks = pages.translit_pages_page_masks
  let flat = pages.translit_pages_page_index_flat
  let pool = pages.translit_pages_pool_strings
  let len = list.length(offsets)
  check_page(0, len, offsets, masks, flat, pool)
}

pub fn check_page(i: Int, len: Int, offsets: List(Int), masks: List(List(Int)), flat: List(Int), pool: List(String)) -> Nil {
  case i == len {
    True -> Nil
    False -> {
      case bits.get_at(offsets, i) {
        Ok(off) -> {
          case off == -1 {
            True -> check_page(i + 1, len, offsets, masks, flat, pool)
            False -> {
              case bits.get_at(masks, i) {
                Ok(words) -> {
                  let total = list.fold(words, 0, fn(acc, w) { acc + bits.popcount(w) })
                  check_entries(off, 0, total, flat, pool)
                  check_page(i + 1, len, offsets, masks, flat, pool)
                }
                Error(_) -> check_page(i + 1, len, offsets, masks, flat, pool)
              }
            }
          }
        }
        Error(_) -> check_page(i + 1, len, offsets, masks, flat, pool)
      }
    }
  }
}

pub fn check_entries(off: Int, idx: Int, total: Int, flat: List(Int), pool: List(String)) -> Nil {
  case idx == total {
    True -> Nil
    False -> {
      case bits.get_at(flat, off + idx) {
        Ok(pi) -> { assert pi >= 0 assert pi < list.length(pool) }
        Error(_) -> { assert list.length(pool) < 0 }
      }
      check_entries(off, idx + 1, total, flat, pool)
    }
  }
}
