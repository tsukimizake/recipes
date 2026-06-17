---
name: recipe-to-cooklang
description: Use when converting recipe files (Markdown, plain text, etc.) in this repo into Cooklang `.cook` format for the custom cooking process scheduler. Covers timer-unit gotchas, atomic step granularity for scheduling, ingredient declaration rules, and metadata conventions.
---

# Recipe to Cooklang Translation

Convert human-readable recipe files (Markdown / plain text) into Cooklang
`.cook` files suitable for the custom cooking process scheduler that consumes
them.

## Output

Place the `.cook` file next to the source recipe, matching the source
filename:

- `chicken-tinga.md` → `chicken-tinga.cook`

Leave the original source file untouched unless the user asks otherwise.

## Cooklang spec quick reference

- Ingredients: `@name{qty%unit}`. Multi-word names need `{}`.
- Inline prep notes: `@name{qty%unit}(prep description)` — only immediately after `{}`.
- Cookware: `#name{}`. Multi-word/non-Latin names need `{}`.
- Timers: `~{qty%unit}` or named `~timername{qty%unit}`.
- Sections: `== Section Name ==`.
- Notes (recipe-level annotations the scheduler can ignore): start a line with `>`.
- Steps: separated by blank lines (one paragraph = one step).
- YAML metadata: front matter between `---` lines at the top.

Spec: <https://cooklang.org/docs/spec/>
Canonical metadata keys: <https://cooklang.org/docs/conventions/>

## Critical gotchas (cooklang-rs will reject otherwise)

**Timer units MUST be English** — the parser validates against an English
enum:

- OK: `~{5%minutes}`, `~{30%seconds}`, `~{1%hours}`
- NG: `~{5%分}`, `~{30%秒}` → `Error: Unknown timer unit`

Other quantity units (`g`, `ml`, `個`, `枚`, `小さじ`, `片`, `本`) are
arbitrary strings and pass through fine. Only timer units are validated.

**Range values in timers (`6-8`, `20-30`) are accepted** as strings. Keep
them when the source recipe specifies a range.

**Names with non-Latin characters need explicit `{}`** because the
word-boundary detection is unreliable for kana/kanji:

- OK: `#中サイズの鍋{}`, `@白玉ねぎ{1%個}`
- Risky: `#中サイズの鍋` may swallow trailing characters into the cookware name.

**Inline prep parens `(...)` attach to the immediately preceding `{}`**.
Don't embed `@` or `#` markup inside an inline prep — it gets treated as
opaque text. Put alternative equipment in a `>` note instead.

## Granularity for the scheduler

The downstream tool is a cooking *process scheduler* — it needs atomic,
sequentially ordered operations with explicit wait/idle time. Optimize for
that.

**One operation per paragraph (= one step).** Split compound actions like
"heat pan → add chicken → sear → flip → remove" into separate paragraphs.

**Group steps under sections** (`== ... ==`) by phase: `下準備`,
`焼き付け`, `煮込み`, `ソースの仕上げ`, `仕上げ`, etc. Sections give the
scheduler natural batching points.

**Make prep steps independent so they can be parallelized.** Each
prepable ingredient gets its own paragraph in the prep section:

```cooklang
== 下準備 ==

@白玉ねぎ{1%個}(小)をみじん切りにする。

@トマティーヨ{170%g}(中2個ほど)の皮を剥く。

@にんにく{4%片}の皮を剥く。
```

Three paragraphs = three independent tasks the scheduler can run concurrently.

**Name long-running timers** so the scheduler can identify and slot parallel
work into them:

- Good: `~煮込み{20-30%minutes}`, `~ソース煮詰め{5%minutes}`
- Anonymous `~{20-30%minutes}` works but is harder to reference.

**Make implicit idle time explicit.** If the recipe says "let the chicken
cool", surface it as its own step or note rather than burying it inside a
sentence — the scheduler can use that window for the next phase.

## Ingredient declaration rules

Declare each ingredient **once with quantity** on first use. Subsequent
mentions are plain text (no `@`):

```cooklang
@鶏胸肉{550%g}(骨つき・皮つき)に塩胡椒をふる。
...
鶏肉を裏返し、さらに~{2%minutes}焼く。  -- 鶏肉 is plain text here
```

Re-using `@鶏胸肉` would double-count it in the auto-generated ingredient
list.

When the source recipe specifies both count and weight (e.g., "2 枚 (合計約
550g)"), prefer the weight for scaling.

## Metadata conventions

Use canonical Cooklang keys (with spaces, not snake_case):

```yaml
---
title: チキン・ティンガ
description: メキシコ風ピリ辛シュレッドチキン
servings: 4
locale: ja
cuisine: メキシカン
course: メイン
prep time: 5 minutes
cook time: 60 minutes
time required: 1h5m
---
```

- `prep time`, `cook time`, `time required` are the canonical keys.
- Use `HhMm` for `time required` to dodge locale/plural issues.
- `locale: ja` enables JP-aware behavior in clients that support it.

## Notes (`>`) for alternatives and caveats

Use `>` lines for information the scheduler should ignore but the cook needs:

```cooklang
> 鶏ガラスープは低塩タイプ(自家製または市販)を使用。水で代用しても可。
> ハンドブレンダーがなければ据え置きのミキサーに移して攪拌する。
```

Equipment alternatives belong here, not in the cookware list — the scheduler
should plan around a single primary tool.

## Validation

After writing, paste the file into the [Cooklang
Playground](https://cooklang.github.io/cooklang-rs/) to confirm it parses
without errors. The most common failure mode is Japanese timer units.

## Reference: working example

`chicken-tinga.cook` in this repo is the canonical example — split into
`下準備` / `鶏肉の焼き付け` / `野菜の焼き付け` / `煮込み` / `ソースの仕上げ`
/ `鶏肉の処理` / `仕上げ` sections, one atomic operation per paragraph, with
named timers for long simmer/reduction phases.
