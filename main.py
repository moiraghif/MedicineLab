import pandas as pd
from radiomics import featureextractor

#nii image reader
import SimpleITK as sitk
import numpy as np

import multiprocessing as mp
import os

#indicating the features required
extract_this = {"shape":      ["Maximum3DDiameter",
                               "MajorAxisLength", "Sphericity",
                               "MinorAxisLength", "SurfaceArea",
                               "SurfaceVolumeRatio",
                               "Flatness", "VoxelVolume"],
                "firstorder": ["Entropy", "Kurtosis", "Maximum",
                               "Mean", "Median", "Minimum",
                               "MeanAbsoluteDeviation",
                               "Skewness", "Variance"],
                "glcm":       ["Autocorrelation", "Contrast"],  # TODO: uncomment
                "glrlm":      ["HighGrayLevelRunEmphasis"],     # TODO: uncomment
                "ngtdm":      ["Contrast", "Coarseness"]}

#initialize the featureextractor and define the required features
extractor = featureextractor.RadiomicsFeatureExtractor()
extractor.disableAllFeatures()
extractor.enableFeaturesByName(**extract_this)

features = ["diagnostics_Mask-original_VoxelNum"]
features_name = ["VoxelNum"]
for key in extract_this.keys():
    for elem in extract_this.get(key):
        features.append("original_" + key + "_" + elem)
        if key == "ngtdm":
            features_name.append(key + "_" + elem)
        else:
            features_name.append(elem)

features_name.append("y")

homImagePath = "./code__esempi/lesions/homogeneous/nifti/"
homImages = [(homImagePath+file, 0) for file in os.listdir(homImagePath)]

hetImagePath = "./code__esempi/lesions/heterogeneous/nifti/"
hetImages = [(hetImagePath+file, 1) for file in os.listdir(hetImagePath)]

images = homImages + hetImages

def get_feature_df(path):
    img    = sitk.ReadImage(path[0])
    mask   = img > 0
    infos  = extractor.execute(img, mask)
    result = [float(infos[f]) for f in features]
    result.append(path[1])
    return result

#some parallelization
pool = mp.Pool(3)
res = pool.map(get_feature_df, images)

#the final df
final_df = pd.DataFrame(res, columns=features_name)

final_df.to_csv("feature_dataset.csv", index=None)  # TODO: uncomment
