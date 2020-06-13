echo "Extracting features..."
python main.py
echo "Training model..."
R -f main.R
