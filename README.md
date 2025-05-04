## Comparative Analysis of Cognitive Modeling and Machine Learning in Predicting Human Behavior of Classifying Real and Fake Images

### Overview
This project investigates how well cognitive models (exemplar and prototype) and machine learning classifiers can predict human judgments when distinguishing real from fake images. We extract facial embeddings using a pre-trained FaceNet model, perform dimensionality reduction, and compare computational model predictions to participant response data.

## Installation

### Prerequisites
- Python 3.8 or higher
- pip
- Both `DATASET` and `DATA AND R CODE` folders from Human Ability Study: https://osf.io/tfn7v/

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
- `FacialEmbeddingPCA .ipynb`: PCA dimensionality reduction and facial embedding generation.
- `ExemplarPrototype.ipynb`: compare human behavior with several machine learning classifiers (from the scikit-learn library).

### Sources
The Facial Embedding Model that we used comes from the following GitHub repository: https://github.com/iamjr15/Facenet-Recognition-PyTorch/blob/master/face-recognition-facenet-svm-pca-visualization.ipynb

All code was directly copied from that repository and used to generate our facial embeddings for every image in the DATASET folder from the OSF study.

The human judgement results was directly copied from the following study: https://osf.io/tfn7v/

We used the `eachresponse.csv` file as training / validation data for our models.

## Data
The only data file from the original Human Ability Study used in this project is located in `DATA AND R CODE/`:
- `eachresponse.csv`: Contains trial-level participant responses, including image IDs, real/fake labels, response times, and confidence ratings.

## Usage
After installing the dependencies and setting up your environment:
1. Launch Jupyter Notebook:
   ```bash
   jupyter notebook
   ```
2. Run the following notebooks in order:
   - **FacialEmbeddingPCA .ipynb**: Generates facial embeddings and performs PCA analysis.
   - **ExemplarPrototype.ipynb**: Fits exemplar and prototype models and compares them (and other classifiers) to human judgments.

## Results
- Dimensionality reduction outputs are saved as pickled DataFrames:
  - `df_pca_*`: PCA projections at various dimensions.
  - `df_nmds_2`: 2D NMDS projections.
  - `df_lda_1`: 1D LDA projections.
- Model performance metrics and visualizations are available within the notebooks.
