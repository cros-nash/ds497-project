## **Comparative Analysis of Cognitive Modeling and Machine Learning in Predicting Human Behavior of Classifying Real and Fake Images**
## Installation

### Prerequisites
- Python 3.8 or higher
- pip
- `DATASET` folder from Human Ability Study: https://osf.io/tfn7v/

### Python setup
1. Create and activate a virtual environment:
   ```bash
   python3 -m venv venv
   source venv/bin/activate
   ```
2. Upgrade pip and install Python dependencies:
   ```bash
   pip install --upgrade pip
   pip install torch torchvision pandas numpy scikit-learn matplotlib seaborn jupyter
   ```

### Jupyter Notebooks
All notebooks are located in the project root:
- `FacialEmbeddingPCA .ipynb`: PCA dimensionality reduction, facial embedding generation and clustering on face embeddings.
- `ExemplarPrototype.ipynb`: compare human behavior with several machine learning classifiers (scikit-learn) on the same task.

```
