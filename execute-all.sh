conda activate medical || \
    (conda env create --file anaconda_environment.yml && conda activate medical)
echo "Extracting features..."
python main.py
echo "Training model..."
R -f main.R
