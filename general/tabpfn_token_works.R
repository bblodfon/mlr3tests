library(reticulate)
# optionally set python path:
use_python(python = "/opt/anaconda3/bin/python", required = TRUE)

# ensure HF token visible to Python
Sys.setenv(HF_TOKEN = Sys.getenv("HF_TOKEN"))

py_run_string("from huggingface_hub import hf_hub_download; print(hf_hub_download('Prior-Labs/tabpfn_2_5','config.json'))")
