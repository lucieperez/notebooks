import ipywidgets as widgets
import pandas as pd
import datetime
from time import time

from IPython.display import display, clear_output


df = pd.read_csv('data/annotation_df_history/isaiah_dataset.csv')
df = df.fillna("")


pd.set_option('display.max_colwidth', None)  # For older versions of pandas, use -1 instead of None
pd.set_option('display.max_columns', None)   # Ensure all columns are shown
pd.set_option('display.max_rows', None)   


# converting 'dir_he' and 'dir_he_dss' from float to int 

df["dir_he"] = df["dir_he"].astype(str)
df["dir_he"] = df["dir_he"].replace("1.0", "1").replace("0.0", "0")

df = df[[ 
    'verb_id',
    'lex',
    'scroll',
    'book',
    'chapter',
    'verse_num',
    'gcons_verb',
    'gcons_verse',
    'sign_info',
    'stem',
    'tense','gcons_clause',
    'subject',
    'complement',
    'cmpl_lex',
    "cmpl_translation",
    'dir_he',
    'cmpl_constr', 
    'cmpl_nt', 
    'cmpl_anim', 
    'cmpl_det', 
    'cmpl_indiv',
    'cmpl_complex',
    'motion_type',
    'preposition_1',
    'preposition_2',
    'preposition_3',
    'preposition_4',
    'comments',
    ]]


# ### Step 3: Define Columns to Annotate and Create Widgets


# List of columns of interest
columns = df.columns.to_list()
columns_for_info = [
    'verb_id',
    'lex',
    'scroll',
    'book',
    'chapter',
    'verse_num',
    'gcons_verb',
    'gcons_verse',
    'sign_info',
    'stem',
    'tense',
]

columns_line2 = [
    'gcons_clause',
    'subject',
    'complement',
    'cmpl_lex',
    "cmpl_translation",
    'dir_he',
    'cmpl_constr', 
    'cmpl_nt', 
    'cmpl_anim', 
    'cmpl_det', 
    'cmpl_indiv',
    'cmpl_complex',
    
]

columns_line3 = [   
    'motion_type',
    'preposition_1',
    'preposition_2',
    'preposition_3',
    'preposition_4',
    'comments',
]

columns_to_annotate = columns_line2 + columns_line3



annotation_input = widgets.Textarea(
    value='',
    placeholder='Type your annotation here',
    description='',
    disabled=False,
    layout=widgets.Layout(width='100%', height='100px')  # Adjust width and height as needed
)

prev_col_button = widgets.Button(
    description='Previous Column',
    disabled=False,
    button_style='primary',
    tooltip='Go to the previous column',
    icon='arrow-left'
)

next_col_button = widgets.Button(
    description='Next Column',
    disabled=False,
    button_style='info',
    tooltip='Go to the next column',
    icon='arrow-right',
)

prev_row_button = widgets.Button(
    description='Previous Row',
    disabled=False,
    button_style='warning',
    tooltip='Go to the previous row',
    icon='arrow-left'
)

next_row_button = widgets.Button(
    description='Next Row',
    disabled=False,
    button_style='success',
    tooltip='Go to the next row',
    icon='arrow-right'
)

# Define annotation buttons for cmpl_constr	cmpl_nt	cmpl_anim	cmpl_det	cmpl_indiv	cmpl_complex

## dir_he

dir_he_buttons = [
    widgets.Button(description='0'),
    widgets.Button(description='1'),
]

## cmpl_constr

cmpl_constr_buttons = [
    widgets.Button(description='prep'),
    widgets.Button(description='dir-he'),
    widgets.Button(description='vc'),
    widgets.Button(description='prep + dir-he'),
]


## cmpl_nt

cmpl_nt_buttons = [
        widgets.Button(description='abs'),
    widgets.Button(description='adv'),
    widgets.Button(description='bopa'),
    widgets.Button(description='dir'),
    widgets.Button(description='gens'),
    widgets.Button(description='obj'),
    widgets.Button(description='occ'),
    widgets.Button(description='other'),
    widgets.Button(description='pers'),
    widgets.Button(description='phen'),
    widgets.Button(description='ppin'),
    widgets.Button(description='prps'),
    widgets.Button(description='topo'),
]

cmpl_nt_buttons = sorted(cmpl_nt_buttons, key=lambda x: x.description) # sort the buttons in alphabetical order


## cmpl_anim

cmpl_anim_buttons = [
    widgets.Button(description='anim'),
    widgets.Button(description='inanim'),
    widgets.Button(description='anim inanim'),
]

## cmpl_det

cmpl_det_buttons = [
    widgets.Button(description='det'),
    widgets.Button(description='und'),
]

## cmpl_indiv

cmpl_indiv_buttons = [
    widgets.Button(description='subs'),
    widgets.Button(description='nmpr'),
    widgets.Button(description='prsf'),
    widgets.Button(description='ppin'),
    widgets.Button(description='adj'),
    widgets.Button(description='adv'),
]

## cmpl_complex

cmpl_complex_buttons = [
    widgets.Button(description='simple'),
    widgets.Button(description='complex'),
]


## motion_type
motion_type_buttons = [
    widgets.Button(description='factive'),
    widgets.Button(description='fictive'),
    widgets.Button(description='factive fictive'),
]

# Get session start index

def get_session_start_index():
    with open('data/annotation_df_history/annotation_tracks.txt') as annotation_file:
        lines = annotation_file.readlines()
        lines_with_index = [line for line in lines if line.startswith("Current Row Index: ")]
        line = lines_with_index[-1]
        return int(line.split()[-1])


# ### Step 4.1 Add function for tracking your progress


# Today's dataframe
df_name = "isaiah_dataset"

# Global variable to store the start index of the annotation session
session_start_index = get_session_start_index()


def save_annotation_details(df, current_index, session_start_index):
    file_path = 'data/annotation_df_history/annotation_tracks.txt'
    current_datetime = datetime.datetime.now().strftime("%Y-%m-%d %H:%M")
    current_row_index = df.index[current_index]

    # Calculate the number of rows annotated in this session
    rows_annotated_this_session = current_index - session_start_index + 1

    details = f"Date and Time: {current_datetime}\nDataFrame: {df_name}\nCurrent Row Index: {current_row_index}\nRows Annotated This Session: {rows_annotated_this_session}\n\n"

    with open(file_path, 'a') as file:
        file.write(details)


def save_dataframe(df, df_name):
    # Get the current date in YYYY-MM-DD format
    current_date = datetime.datetime.now().strftime("%Y-%m-%d_%H_%M")

    # Construct the filename
    filename = f"data/annotation_df_history/{df_name}_{current_date}.csv"
    current_file_name = f"data/annotation_df_history/{df_name}.csv"

    # Save the DataFrame to a CSV file
    df.to_csv(filename, index=False)  # Set index=False if you don't want to save the index
    df.to_csv(current_file_name, index=False)

    print(f"DataFrame saved as {filename} and {current_file_name}.")


# Create event handler functions for specific annotation buttons

# dir_he

def set_dir_he(button):
    global current_column_index
    # Set the annotation value
    annotation_input.value = button.description
    # Save the current annotation
    df.at[current_index, columns_to_annotate[current_column_index]] = annotation_input.value

    # Move to the next column
    if current_column_index + 1 < len(columns_to_annotate):
        current_column_index += 1
        display_row(current_index, current_column_index)
    else:
        print("All annotations for this row are complete.")
        # Optional: Reset column index or navigate to the next row

    
for button in dir_he_buttons:
    button.on_click(set_dir_he)

# cmpl_const

def set_cmpl_constr(button):
    global current_column_index
    # Set the annotation value
    annotation_input.value = button.description
    # Save the current annotation
    df.at[current_index, columns_to_annotate[current_column_index]] = annotation_input.value

    # Move to the next column
    if current_column_index + 1 < len(columns_to_annotate):
        current_column_index += 1
        display_row(current_index, current_column_index)
    else:
        print("All annotations for this row are complete.")
        # Optional: Reset column index or navigate to the next row
    
for button in cmpl_constr_buttons:
    button.on_click(set_cmpl_constr)
    
# cmpl_nt

def set_cmpl_nt(button):
    global current_column_index
    # Set the annotation value
    annotation_input.value = button.description
    # Save the current annotation
    df.at[current_index, columns_to_annotate[current_column_index]] = annotation_input.value

    # Move to the next column
    if current_column_index + 1 < len(columns_to_annotate):
        current_column_index += 1
        display_row(current_index, current_column_index)
    else:
        print("All annotations for this row are complete.")
        # Optional: Reset column index or navigate to the next row
    
for button in cmpl_nt_buttons:
    button.on_click(set_cmpl_nt)
      
# cmpl_anim

def set_cmpl_anim(button):
    global current_column_index
    # Set the annotation value
    annotation_input.value = button.description
    # Save the current annotation
    df.at[current_index, columns_to_annotate[current_column_index]] = annotation_input.value

    # Move to the next column
    if current_column_index + 1 < len(columns_to_annotate):
        current_column_index += 1
        display_row(current_index, current_column_index)
    else:
        print("All annotations for this row are complete.")
        # Optional: Reset column index or navigate to the next row
    
for button in cmpl_anim_buttons:
    button.on_click(set_cmpl_anim)
    
# cmpl_det

def set_cmpl_det(button):
    global current_column_index
    # Set the annotation value
    annotation_input.value = button.description
    # Save the current annotation
    df.at[current_index, columns_to_annotate[current_column_index]] = annotation_input.value

    # Move to the next column
    if current_column_index + 1 < len(columns_to_annotate):
        current_column_index += 1
        display_row(current_index, current_column_index)
    else:
        print("All annotations for this row are complete.")
        # Optional: Reset column index or navigate to the next row
        
for button in cmpl_det_buttons:
    button.on_click(set_cmpl_det)
    
# cmpl_indiv

def set_cmpl_indiv(button):
    global current_column_index
    # Set the annotation value
    annotation_input.value = button.description
    # Save the current annotation
    df.at[current_index, columns_to_annotate[current_column_index]] = annotation_input.value

    # Move to the next column
    if current_column_index + 1 < len(columns_to_annotate):
        current_column_index += 1
        display_row(current_index, current_column_index)
    else:
        print("All annotations for this row are complete.")
        # Optional: Reset column index or navigate to the next row
    
for button in cmpl_indiv_buttons:
    button.on_click(set_cmpl_indiv)
    
# cmpl_complex

def set_cmpl_complex(button):
    global current_column_index
    # Set the annotation value
    annotation_input.value = button.description
    # Save the current annotation
    df.at[current_index, columns_to_annotate[current_column_index]] = annotation_input.value

    # Move to the next column
    if current_column_index + 1 < len(columns_to_annotate):
        current_column_index += 1
        display_row(current_index, current_column_index)
    else:
        print("All annotations for this row are complete.")
        # Optional: Reset column index or navigate to the next row
    
for button in cmpl_complex_buttons:
    button.on_click(set_cmpl_complex)
    
# motion_type

def set_motion_type(button):
    global current_column_index
    # Set the annotation value
    annotation_input.value = button.description
    # Save the current annotation
    df.at[current_index, columns_to_annotate[current_column_index]] = annotation_input.value

    # Move to the next column
    if current_column_index + 1 < len(columns_to_annotate):
        current_column_index += 1
        display_row(current_index, current_column_index)
    else:
        print("All annotations for this row are complete.")
        # Optional: Reset column index or navigate to the next row
    
for button in motion_type_buttons:
    button.on_click(set_motion_type)


# ### Step 4.2 Functions to Display Data and Widgets with Navigation and Iteration Logic

def display_row(row_index, col_index):
    global current_index, current_column_index
    current_index, current_column_index = row_index, col_index
    
    # Get the value from the DataFrame, convert NaN or non-string values to string
    cell_value = df.at[row_index, columns_to_annotate[col_index]]
    if pd.isna(cell_value):
        cell_value = ''  # Convert NaN to an empty string
    else:
        cell_value = str(cell_value)  # Convert non-string values to string

    clear_output(wait=True)

    # Set the value of the annotation input and display it along with other elements
    annotation_input.value = cell_value
    display(df.iloc[row_index:row_index+1][columns_for_info])
    display(df.iloc[row_index:row_index+1][columns_line2])
    display(df.iloc[row_index:row_index+1][columns_line3])
    #display(df.iloc[row_index][columns_for_info])
    #display(df.iloc[row_index][columns_to_annotate])
    display(widgets.HTML(value=f"<b>Annotate '{columns_to_annotate[col_index]}':</b>"))
    
    # Add the annotation buttons for specific columns:
    
        
    if columns_to_annotate[col_index] == "dir_he":
        display(widgets.HBox(dir_he_buttons))
        
    if columns_to_annotate[col_index] == "cmpl_constr":
        display(widgets.HBox(cmpl_constr_buttons))
        
    if columns_to_annotate[col_index] == "cmpl_nt":
        display(widgets.HBox(cmpl_nt_buttons))

    if columns_to_annotate[col_index] == "cmpl_anim":
        display(widgets.HBox(cmpl_anim_buttons))
        
    if columns_to_annotate[col_index] == "cmpl_det":
        display(widgets.HBox(cmpl_det_buttons))
        
    if columns_to_annotate[col_index] == "cmpl_indiv":
        display(widgets.HBox(cmpl_indiv_buttons))
        
    if columns_to_annotate[col_index] == "cmpl_complex":
        display(widgets.HBox(cmpl_complex_buttons))  
    
    if columns_to_annotate[col_index] == 'motion_type':
        display(widgets.HBox(motion_type_buttons))
 
    
    display(annotation_input)
    display(widgets.HBox([prev_col_button, next_col_button]))
    display(widgets.HBox([prev_row_button, next_row_button]))

    
def navigate_row(offset):
    new_index = current_index + offset
    if 0 <= new_index < len(df):
        display_row(new_index, 0)


# ### Step 5: Handle Annotation Submission and Row Navigation

# In[19]:


def on_prev_col_clicked(b):    
    global current_column_index
    # Save the current annotation
    df.at[current_index, columns_to_annotate[current_column_index]] = annotation_input.value

    # Navigate to the previous column
    if current_column_index > 0:
        current_column_index -= 1
        display_row(current_index, current_column_index)

        
def on_next_col_clicked(b):
    global current_column_index
    # Save the current annotation
    df.at[current_index, columns_to_annotate[current_column_index]] = annotation_input.value

    # Check if the current column is the last one in the row
    if current_column_index + 1 < len(columns_to_annotate):
        # Not the last column, move to the next column
        current_column_index += 1
        display_row(current_index, current_column_index)
    else:
        # Last column, all annotations for this row are complete
        print("All annotations for this row are complete.")
        # Optional: Reset column index or navigate to the next row

        
def on_prev_row_clicked(b):
    navigate_row(-1)

def on_next_row_clicked(b):
    navigate_row(1)



if "on_click_defined" not in globals():
    on_click_defined = True
    prev_col_button.on_click(on_prev_col_clicked)
    next_col_button.on_click(on_next_col_clicked)

    prev_row_button.on_click(on_prev_row_clicked)
    next_row_button.on_click(on_next_row_clicked)
    
current_index = get_session_start_index()

def get_current_index():
    return current_index