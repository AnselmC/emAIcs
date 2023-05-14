echo "Making sure git lfs is available + installed..."
git lfs install
REPO_URL="https://huggingface.co/tloen/alpaca-lora-7b"
LOCAL_REPO_DIR="alpaca-lora-7b"

echo "Cloning repository ${REPO_URL}..."
git clone $REPO_URL $LOCAL_REPO_DIR
