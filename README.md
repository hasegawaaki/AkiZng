# Local RAG Agent with Gemini CLI & LangChain

This project implements a conversational, retrieval-augmented generation (RAG) agent that runs locally. It leverages a knowledge base of your own documents, uses LangChain for document processing, and uniquely calls the local Gemini CLI for text generation. The entire experience is wrapped in a user-friendly Gradio web interface.

The agent can answer questions based on the documents you provide and remembers the context of the conversation, allowing for natural, follow-up questions.

## ✨ Features

-   **Conversational Memory:** The agent remembers previous turns in the conversation to provide contextually relevant answers.
-   **Retrieval-Augmented Generation (RAG):** Answers are grounded in a local knowledge base. The agent first retrieves relevant information before generating a response.
-   **Local Gemini CLI Integration:** Instead of using a Python SDK for generation, this agent directly calls the installed Gemini CLI on your local machine via a PowerShell command.
-   **Local Vector Store:** Uses FAISS, a fast and efficient similarity search library, to store document embeddings locally. No need for a cloud-based vector database.
-   **Flexible Knowledge Base:** Easily add your own PDF documents to the `knowledge-base` directory to customize the agent's expertise.
-   **Interactive UI:** A simple and clean chat interface powered by Gradio.

## ⚙️ How It Works

The project follows a standard RAG pipeline, but with a unique final step for generation:

1.  **Load Documents:** The application scans the `knowledge-base` directory, loading all PDF files it finds.
2.  **Chunk Documents:** The loaded documents are split into smaller, more manageable chunks of text. This helps the retrieval process find more specific and relevant context.
3.  **Create Embeddings:** Each text chunk is converted into a numerical representation (an embedding) using Google's `embedding-001` model.
4.  **Store in Vector DB:** These embeddings are stored in a local FAISS vector store. This allows for rapid and efficient searching of the most relevant text chunks based on a user's query.
5.  **Process Query:** When you ask a question:
    a. The agent retrieves the most relevant document chunks from the FAISS vector store.
    b. It constructs a detailed prompt containing the conversation history, the retrieved context from the knowledge base, and your new question.
    c. It executes a PowerShell command to call the local Gemini CLI (`gemini.ps1`), passing this detailed prompt as input.
6.  **Generate & Display Response:** The Gemini CLI processes the prompt and generates a response. This response is captured and displayed in the Gradio chat interface.

## 🚀 Getting Started

Follow these steps to set up and run the project on your local machine.

### Prerequisites

-   Python 3.8+
-   [Google AI Gemini CLI](https://ai.google.dev/docs/gemini_cli_quickstart) installed and authenticated.
-   An environment variable set for your Google API Key.

### 1. Clone the Repository

```bash
git clone <your-repo-url>
cd <your-repo-name>

### 2. Install Dependencies

```python
# Create a virtual environment
python -m venv .venv
source .venv/bin/activate  # On Windows, use `.venv\Scripts\activate`

# Install the required packages
pip install -r requirements.txt

### 3. Set Up Your API Key

Create a file named `.env` in the root of the project directory and add your Google API key:

```python
GOOGLE_API_KEY="your-api-key-here"

### 4. Configure the Gemini CLI Path

In the `query_with_gemini_cli` function within the script, you **must** update the `gemini_script_path` variable to point to the location of your `gemini.ps1` file.

```python
# Find this line in your script and change the path
gemini_script_path = "C:\\Users\\YourUsername\\AppData\\Roaming\\npm\\gemini.ps1" # <--- CHANGE THIS

### 5. Create Your Knowledge Base

1.  Create a directory named `knowledge-base` in the root of the project.
2.  Inside `knowledge-base`, you can create subdirectories for different document types (e.g., `project-docs`, `meeting-notes`).
3.  Place your PDF files inside these subdirectories. The script will load all of them automatically.

### 6. Run the Application

Execute the Jupyter Notebook or run the equivalent Python script.

```bash
jupyter notebook RAGCLI.ipynb

