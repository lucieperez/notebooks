# Need a folder test at the root ot the project
# create a .py file, name starts with test
# each function title starts with test and explain what you test


import os
import pytest
import pandas as pd


ROOT_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DATA_FOLDER = "1_annotation_tools/data/annotation_df_history"
DATASET = "isaiah_dataset.csv"



@pytest.fixture(scope="module")
def input_df():
    df = pd.read_csv(os.path.join(ROOT_DIR, DATA_FOLDER, DATASET))
    return df


def test_lines(input_df):
    assert input_df.shape[0] > 0


def test_motion_type_allowed_values(input_df):
    allowed_values = {"factive", "fictive", "factive fictive", "other", "unknown"}
    assert all([value in allowed_values for value in set(input_df.motion_type) if isinstance(value, str)])


def test_len_g_cons_verse_and_len_sign_info_are_equal(input_df):
    assert all([len(text_str) == len(sign_str) for text_str, sign_str in zip(input_df.gcons_verse, input_df.sign_info) if isinstance(sign_str, str)])
