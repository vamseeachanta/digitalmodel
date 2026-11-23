# Plan: digitalmodel #1038 — Evaluate baidu/Unlimited-OCR (DeepSeek-OCR, MIT) as the scanned-doc OCR engine

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1038
**Epic:** https://github.com/vamseeachanta/digitalmodel/issues/1011 (file-format adapter review)
**Companion:** https://github.com/vamseeachanta/digitalmodel/issues/1014 (Office/other document adapter matrix), https://github.com/vamseeachanta/digitalmodel/issues/1016 (cad/ package)
**Status:** plan-review
**Tier:** T1 (planning document only — no code, no model execution, no GPU required to produce this doc)
**Client:** N/A
**Project:** N/A
**Lane:** claude

> **This is a setup + benchmark PLAN, not an execution record.** Nothing here runs a model. The numbered protocol below is to be executed later on a GPU box by an operator. Version pins and commands are quoted from the upstream repo / model card as of 2026-06-26 and must still be re-verified on the box (`[to confirm on box]`).

---

## 0. Motivation

The ACMA ingest/extraction pipeline validated the poppler / office / `.wbpz` paths, but **tesseract failed on scanned/raster PDFs**. Scanned documents and raster engineering pages are therefore a hard gap in the document-adapter layer (#1014) and, downstream, the CAD adapter package (#1016). DeepSeek-OCR — and specifically the MIT-licensed [`baidu/Unlimited-OCR`](https://github.com/baidu/Unlimited-OCR) wrapper around it — is a layout-aware, multi-page, transformer-based candidate to fill that gap.

This plan defines: hardware needed, install steps, a benchmark protocol against the exact files tesseract failed on, a decision gate, a thin adapter sketch, and risks.

---

## 1. Hardware requirements

### 1.1 What the upstream sources say

- **Unlimited-OCR README** states the stack was *"tested on python 3.12.3 + CUDA 12.9"* with `torch==2.10.0`. It asserts an **NVIDIA GPU** is required but **states no explicit VRAM minimum**. `[confirmed: no VRAM number in README]`
- **DeepSeek-OCR model card** (`deepseek-ai/DeepSeek-OCR`) describes a **~3B-parameter** model (the card lists tensor type **BF16**, safetensors), tested on *"python 3.12.9 + CUDA 11.8"* with `flash-attn==2.7.3`. No explicit VRAM minimum on the card either.

### 1.2 Derived VRAM estimate (headline requirement)

DeepSeek-OCR is ~3.3B params. At BF16/FP16 the **weights alone are ≈ 6.6–7 GB**; with activations + KV cache for the 32,768-token context, community guidance puts single-stream inference at **≈ 7–8 GB VRAM**, and recommends **16 GB for comfortable headroom**, **24 GB for large/complex multi-page documents or batch/concurrent inference**. INT4 quantization can drop weights to ~1.8 GB but is **out of scope for the accuracy benchmark** (quantization would confound the accuracy result).

**Headline requirement → plan on a 24 GB-class NVIDIA GPU** (e.g. RTX 3090/4090/A5000/A10/L4-class or better) to run gundam mode + 1024 px + 32k context + `--concurrency 8` without OOM. A 16 GB card is the practical floor for single-stream base/640 px work. `[to confirm on box: actual peak VRAM at each mode — this is a benchmark output, see §3]`

### 1.3 CPU / RAM / disk

- **Disk:** model weights ~7 GB (BF16 safetensors) + CUDA/torch wheels (several GB) → reserve **~30 GB** free. `[to confirm on box]`
- **RAM:** ≥ 32 GB system RAM recommended for PDF rasterization (`pymupdf`) of large multi-page docs + concurrency. `[to confirm on box]`
- **CUDA:** repo tested on **CUDA 12.9**; DeepSeek-OCR card on 11.8. Driver must support the torch CUDA build chosen. `[to confirm on box]`

### 1.4 Which of our machines could host it — GPU gap flag

- **ace-linux-1 / ace-linux-2 (Linux dev boxes):** primary candidates for the *plan's* execution because they have native `/mnt/ace` access to the test files, **but they are CPU-class dev boxes and may lack a CUDA GPU** with adequate VRAM. `[to confirm: do either dev box have an NVIDIA GPU ≥ 16 GB? Run `nvidia-smi` on each]`
- **Licensed Windows box (ace-win-2, OrcaFlex/OrcaWave host):** has GPU-class hardware but is kept deliberately thin and is Windows (CUDA 12.9 + the Linux-style `infer.py` path needs validation on Windows). Not the first choice. `[to confirm]`
- **Fallback:** a **rented cloud GPU** (24 GB-class, ~$0.5–1.5/hr) for the one-off benchmark, with the de-identified test subset copied up. This is the most likely path if no dev box has a GPU. `[to confirm — see Risks §6]`

> **GPU-availability is the gating unknown for this whole evaluation.** Resolve §1.4 first; everything else assumes a working CUDA GPU.

---

## 2. Install

The README's install instructions are **clearly documented** (explicit pinned `requirements` + HF and SGLang code/commands), so install is low-risk. Two minor inconsistencies were noted upstream (see ⚠️ below) and must be resolved on the box.

### 2.1 Environment (per Unlimited-OCR README, verbatim pins)

```
# tested: python 3.12.3 + CUDA 12.9
torch==2.10.0
torchvision==0.25.0
transformers==4.57.1
Pillow==12.1.1
matplotlib==3.10.8
einops==0.8.2
addict==2.4.0
easydict==1.13
pymupdf==1.27.2.2
psutil==7.2.2
```

Per the workspace Python rule, drive this through `uv` (`uv venv --python 3.12` → `uv pip install ...`). `[to confirm on box: torch 2.10.0 + CUDA 12.9 wheel availability for the box's driver]`

> Note: the DeepSeek-OCR *model card* pins an older stack (`torch==2.6.0`, `transformers==4.46.3`, `flash-attn==2.7.3`). **Use the Unlimited-OCR pins** (it is the wrapper we are evaluating); only fall back to the DeepSeek-OCR pins if the Unlimited-OCR stack fails to build. `[to confirm: does Unlimited-OCR require flash-attn? README requirements list does not include it — verify whether the model code imports flash_attention_2 at runtime]`

### 2.2 Path A — HF transformers (do this first; simplest)

Model ID: `baidu/Unlimited-OCR` (MIT). Verbatim usage from README:

```python
import torch
from transformers import AutoModel, AutoTokenizer

model_name = 'baidu/Unlimited-OCR'
tokenizer = AutoTokenizer.from_pretrained(model_name, trust_remote_code=True)
model = AutoModel.from_pretrained(
    model_name, trust_remote_code=True,
    use_safetensors=True, torch_dtype=torch.bfloat16
).eval().cuda()

# Single image
model.infer(
    tokenizer, prompt='<image>document parsing.',
    image_file='your_image.jpg', output_path='your/output/dir',
    base_size=1024, image_size=640, crop_mode=True,   # gundam mode
    max_length=32768, no_repeat_ngram_size=35, ngram_window=128,
    save_results=True
)
```

Weights download on first `from_pretrained` (HF cache). Modes:
- **gundam:** `base_size=1024, image_size=640, crop_mode=True` (single + the higher-fidelity mode)
- **base:** `base_size=1024, image_size=1024, crop_mode=False` (also the mode used for multi-page/PDF)
- multi-page uses `ngram_window=1024` (vs `128` single image).

Batch CLI (verbatim):
```shell
# image directory
python infer.py --image_dir ./examples/images --output_dir ./outputs --concurrency 8 --image_mode gundam
# PDF
python infer.py --pdf ./examples/document.pdf --output_dir ./outputs --concurrency 8 --image_mode gundam
```
Other flags: `--model_dir`, `--gpu`, `--server_log`.

### 2.3 Path B — SGLang streaming endpoint (optional; only if Path A passes the gate)

README server launch (verbatim):
```shell
python -m sglang.launch_server \
    --model baidu/Unlimited-OCR \
    --served-model-name Unlimited-OCR \
    --attention-backend fa3 --page-size 1 \
    --mem-fraction-static 0.8 --context-length 32768 \
    --enable-custom-logit-processor \
    --disable-overlap-schedule --skip-server-warmup \
    --host 0.0.0.0 --port 10000
```
Client = OpenAI-compatible `POST http://127.0.0.1:10000/v1/chat/completions` with base64 images and the `DeepseekOCRNoRepeatNGramLogitProcessor` custom logit processor.

> ⚠️ SGLang install caveat from README: *"install the local SGLang wheel first, then pin `kernels==0.9.0`"* — but a later passage mentions `kernels==0.11.7`. **Resolve this conflict on the box** before timing the SGLang path. `[to confirm on box]` SGLang/fa3 also implies a recent CUDA + compatible GPU arch.

---

## 3. Benchmark protocol

### 3.1 Test set (the exact files tesseract failed on)

The corpus is the **ACMA ingest/extraction campaign op-set** — the scanned/raster PDFs that the validated tesseract step failed to extract, plus a few engineering drawings that contain text. These live on the **ace share** (op-set staging used by the ACMA campaign). `[to confirm path on box — do not commit the raw share path; digitalmodel is PUBLIC]`

Construct the evaluation subset:
1. From the ACMA op-set, select **10–15 representative scanned/raster PDF pages** spanning: dense typed scans, mixed text+table pages, and ≥ 2 engineering drawings with callout/title-block text.
2. **Hand-key ground truth** for a smaller **3–5 page** subset (typed text + one table) for CER/WER. Drawings get qualitative scoring only (see 3.3). Keep ground-truth files **off the public repo** (store with the de-identified subset). `[to confirm path]`
3. Copy the de-identified subset to the GPU box (or cloud GPU).

### 3.2 Runs to perform

| Run | Engine | Mode / settings |
|-----|--------|-----------------|
| R0 | **tesseract** (baseline) | current ACMA config — reproduce the known failure |
| R1 | Unlimited-OCR HF | **base** (`1024/1024`, crop off) |
| R2 | Unlimited-OCR HF | **gundam** (`1024/640`, crop on) |
| R3 | Unlimited-OCR HF | multi-page PDF (base, `ngram_window=1024`), `--concurrency 8` |
| R4 (opt) | Unlimited-OCR SGLang | streaming endpoint, throughput run |

### 3.3 Metrics

- **Text accuracy:** **CER** and **WER** vs hand-keyed ground truth (3–5 pages). Report per-page and mean.
- **Layout fidelity:** does output preserve reading order, headings, paragraph/column structure? Score 0–3 per page (rubric: 0 garbled, 1 text-only no structure, 2 mostly-correct structure, 3 faithful).
- **Table handling:** for the table page — cells recovered correctly (count) / row+column structure preserved (yes/partial/no).
- **Figure/drawing handling:** for the engineering drawings — title-block + callout text recovered (qualitative: usable / partial / unusable); **explicitly flag any hallucinated text** (see Risks §6).
- **Throughput:** **pages/min** for R3 (batch) and R4 (streaming).
- **VRAM peak:** `nvidia-smi`/`torch.cuda.max_memory_allocated()` peak per mode — **this fills the §1.2 `[to confirm]`**.

### 3.4 Output

A results table (one row per run) + the raw OCR outputs (de-identified) attached to #1038 as a follow-up comment. Do **not** commit raw client text to the public repo.

---

## 4. Success criteria & decision gate

**Adopt Unlimited-OCR as the OCR adapter in the #1014 matrix (and reference it for #1016)** iff **all** of:

1. **Accuracy beats the gap:** on the hand-keyed typed-text pages, **CER ≤ 5%** (target) / **≤ 10%** (acceptable floor) — and, decisively, **succeeds where tesseract (R0) failed/produced garbage** on the scanned pages.
2. **Layout usable:** mean layout-fidelity score **≥ 2/3** and the table page recovered at **≥ "partial"** structure.
3. **No disqualifying hallucination:** on engineering drawings, **zero confidently-wrong fabricated strings** in title-block/callout extraction (a hallucinated dimension/spec is worse than a blank — see §6). Partial/blank is acceptable; confidently-wrong is a fail for the drawing use-case.
4. **Operationally feasible:** runs within available/affordable GPU (single 24 GB-class GPU, batch throughput ≥ ~1 page/min sustained) **and** peak VRAM fits the chosen host.

**Conditional adopt:** if 1–2 pass but 3 fails *only on drawings*, adopt as the **scanned-text-PDF** adapter (the actual tesseract gap) and **exclude raster engineering drawings** from its scope, routing those to a human/CAD-specific path. This still closes the primary #1014 gap.

**Reject** if: CER > 10% on typed text, OR it cannot beat tesseract on the failing scans, OR GPU cost/availability is prohibitive with no acceptable cloud fallback.

---

## 5. Integration sketch (conceptual — no implementation)

Plug into the document-adapter layer consistent with how #1013/#1014 frame adapters (format → read?/convert?/license?/headless? → tool). A thin OCR adapter wrapping Unlimited-OCR:

```python
# conceptual contract — NOT to be implemented in this plan
@dataclass
class OcrResult:
    text: str                      # layout-preserving markdown/plaintext
    pages: list[PageResult]        # per-page text + bbox/layout blocks
    engine: str                    # "unlimited-ocr"
    mode: str                      # "base" | "gundam"
    meta: dict                     # model id, px settings, vram_peak, timings

def ocr_document(
    path: Path,                    # image or PDF (raster/scanned)
    *,
    mode: Literal["base", "gundam"] = "gundam",
    image_size: int = 640,         # 640 | 1024
    max_length: int = 32768,
    concurrency: int = 8,
    backend: Literal["hf", "sglang"] = "hf",
) -> OcrResult: ...
```

- **Placement:** a small `ocr` adapter sitting beside the office/document readers feeding #1014's matrix; surfaced to #1016's `cad/` package only as a *text-extraction-from-raster* helper (not a geometry reader).
- **Capability flags for the #1014 matrix:** read = yes (raster/scanned PDF + images); convert = text/markdown; license = **MIT (headless-OK)**; headless = yes but **GPU-required** (this is the matrix's key caveat vs CPU tesseract).
- **Fallback chain:** route to Unlimited-OCR only when the cheap path (text-layer extraction via poppler/pymupdf) yields no text — i.e. it is the *scanned-PDF* branch, not the default.
- Conceptual only; no code is written under this issue.

---

## 6. Risks

1. **License:** **MIT confirmed** on both the Unlimited-OCR repo and the DeepSeek-OCR model card — headless/commercial use is fine. `[re-confirm the LICENSE file on clone — low risk]`
2. **GPU availability / cost:** the dominant risk. Dev boxes may have **no adequate CUDA GPU** (§1.4); the licensed Windows box is thin/Windows. Likely need a **rented 24 GB cloud GPU** for the benchmark (~$0.5–1.5/hr, one-off). Ongoing production OCR would need a standing GPU or per-job cloud burst — factor into the adopt decision.
3. **Hallucination on engineering drawings:** transformer OCR/VLMs can **fabricate plausible-but-wrong** text (dimensions, part numbers, specs). For engineering content a confidently-wrong number is more dangerous than a blank. Mitigated by the §4 gate criterion 3 and the conditional-adopt (drawings-excluded) fallback.
4. **Multi-page context limit:** 32,768-token context bounds how much a single one-shot pass can hold; very long documents must be **chunked by page batches** (the `infer.py --pdf` path + `ngram_window=1024` handles paging, but verify behavior past the context limit). `[to confirm on box: behavior on a 100+ page scanned PDF]`
5. **Version/stack fragility:** bleeding-edge pins (`torch==2.10.0`, `transformers==4.57.1`, CUDA 12.9) + the SGLang `kernels==0.9.0` vs `0.11.7` README inconsistency. Build the HF path first; treat SGLang as optional. `[to confirm on box]`
6. **PUBLIC-repo data hygiene:** test files are client/share data — keep the corpus, ground truth, and raw OCR output **off** the public digitalmodel repo; commit only metrics/aggregates. Never commit raw share paths.

---

## 7. Execution order (for the operator on the GPU box)

1. Resolve §1.4 — find a CUDA GPU (`nvidia-smi` on dev boxes; else rent). **Gate: no GPU → stop, escalate.**
2. Install env (§2.1) via `uv`; resolve flash-attn/kernels questions. `[to confirm]`
3. Path A HF smoke test on one README example image.
4. Stage de-identified ACMA op-set subset + hand-keyed ground truth (§3.1). `[to confirm path]`
5. Run R0–R3 (and R4 if A passes), capture §3.3 metrics incl. VRAM peak.
6. Score against §4 gate → post results + recommendation to #1038.

---

### Open `[to confirm on box]` items (consolidated)
- Actual peak VRAM per mode (§1.2 / §3.3) — currently estimated 7–8 GB single-stream, 16–24 GB recommended.
- Whether either Linux dev box has an NVIDIA GPU ≥ 16 GB (`nvidia-smi`) (§1.4).
- Disk/RAM headroom on chosen host (§1.3).
- torch 2.10.0 + CUDA 12.9 wheel availability for the box's driver (§2.1).
- Whether Unlimited-OCR needs `flash-attn` at runtime (not in its requirements list) (§2.1).
- SGLang `kernels` pin: `0.9.0` vs `0.11.7` (§2.3).
- ACMA op-set / ground-truth paths on the ace share (§3.1).
- Behavior on 100+ page scanned PDFs past the 32k context (§6.4).
