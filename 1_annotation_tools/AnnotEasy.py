import ipywidgets as widgets
import pandas as pd
import datetime
from IPython.display import display, clear_output

class AnnotationTool:
    def __init__(self, csv_file_path, history_file_path):
        self.df = pd.read_csv(csv_file_path)
        self.df.fillna("", inplace=True)
        self.history_file_path = history_file_path
        self.current_index = self.get_session_start_index()
        self.current_column_index = 0
        self.initialize_widgets()
        self.configure_dataframe()
        
        # Initialize column lists
        self.columns = self.df.columns.to_list()
        self.columns_for_info = [
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

        self.columns_line2 = [
            'gcons_clause', 
            'subject', 
            'complement', 
            'cmpl_lex', 
            'cmpl_translation',
            'dir_he', 
            'cmpl_constr', 
            'cmpl_nt', 
            'cmpl_anim', 
            'cmpl_det', 
            'cmpl_indiv', 
            'cmpl_complex',
        ]

        self.columns_line3 = [   
            'motion_type', 
            'preposition_1', 
            'preposition_2', 
            'preposition_3', 
            'preposition_4', 
            'comments',
        ]

        self.columns_to_annotate = self.columns_line2 + self.columns_line3

    def configure_dataframe(self):
        self.df = self.df.fillna("")
        self.df["dir_he"] = self.df["dir_he"].astype(str)
        self.df["dir_he"] = self.df["dir_he"].replace("1.0", "1").replace("0.0", "0")
        self.df = self.df[[ 
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
        

    def initialize_widgets(self):
        
        # Annotation display area
        self.annotation_input = widgets.Textarea(
            value='',
            placeholder='Type your annotation here',
            description='',
            disabled=False,
            layout=widgets.Layout(width='100%', height='100px'),
        )
        
        # PREVIOUS COLUMN
        # Button
        self.prev_col_button = widgets.Button(
            description='Previous Column',
            disabled=False,
            button_style='primary',
            tooltip='Go to the previous column',
            icon='arrow-left',
        )
            
        # Even handler
        self.prev_col_button.on_click(self.on_prev_col_clicked)
        
        # NEXT COLUMN  
        # Button
        self.next_col_button = widgets.Button(
            description='Next Column',
            disabled=False,
            button_style='info',
            tooltip='Go to the next column',
            icon='arrow-right',
        )
        
        # Event handler
        self.next_col_button.on_click(self.on_next_col_clicked)   
        
        # PREVIOUS ROW
        # Button
        self.prev_row_button = widgets.Button(
            description='Previous Row',
            disabled=False,
            button_style='warning',
            tooltip='Go to the previous row',
            icon='arrow-left'
        )
            
        # Event handler
        self.prev_row_button.on_click(self.on_prev_row_clicked)
           
        # NEXT ROW
        # Button
        self.next_row_button = widgets.Button(
            description='Next Row',
            disabled=False,
            button_style='success',
            tooltip='Go to the next row',
            icon='arrow-right'
        )
        # Event handler
        self.next_row_button.on_click(self.on_next_row_clicked)
        
        # DIR_HE
        # Buttons
        self.dir_he_buttons = [
            widgets.Button(description='0'),
            widgets.Button(description='1'),
        ]
        
        # Even handler
        for button in self.dir_he_buttons:
            button.on_click(self.set_value)
            
        # CMPL_CONSTR
        # Buttons
        self.cmpl_constr_buttons = [
            widgets.Button(description='prep'),
            widgets.Button(description='dir-he'),
            widgets.Button(description='vc'),
            widgets.Button(description='prep + dir-he'),
        ]
        # Event handler
        for button in self.cmpl_constr_buttons:
            button.on_click(self.set_value)

        # CMPL_NT
        # Buttons
        self.cmpl_nt_buttons = [
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
        
        # Even handler
        for button in self.cmpl_nt_buttons:
            button.on_click(self.set_value)


        # CMPL_ANIM
        # Buttons
        self.cmpl_anim_buttons = [
            widgets.Button(description='anim'),
            widgets.Button(description='inanim'),
            widgets.Button(description='anim inanim'),
        ]
        
        # Event handler
        for button in self.cmpl_anim_buttons:
            button.on_click(self.set_value)
       
            
        # CMPL_DET
        # Buttons
        self.cmpl_det_buttons = [
            widgets.Button(description='det'),
            widgets.Button(description='und'),
        ]
        
        # Event handler
        for button in self.cmpl_det_buttons:
            button.on_click(self.set_value)

        # CMPL_INDIV
        # Buttons:
        self.cmpl_indiv_buttons = [
            widgets.Button(description='subs'),
            widgets.Button(description='nmpr'),
            widgets.Button(description='prsf'),
            widgets.Button(description='ppin'),
            widgets.Button(description='adj'),
            widgets.Button(description='adv'),
        ]
        
        # Even handler
        for button in self.cmpl_indiv_buttons:
            button.on_click(self.set_value)
            
        # CMPL_COMPLEX
        # Buttons
        self.cmpl_complex_buttons = [
            widgets.Button(description='simple'),
            widgets.Button(description='complex'),
        ]
        
        # Event handler
        for button in self.cmpl_complex_buttons:
            button.on_click(self.set_value)


        # MOTION_TYPE
        # Buttons
        self.motion_type_buttons = [
            widgets.Button(description='factive'),
            widgets.Button(description='fictive'),
            widgets.Button(description='factive fictive'),
        ]
            
        # Event handler
        for button in self.motion_type_buttons:
            button.on_click(self.set_value)
            
        # TRANSLATION
        # Buttons
        self.prev_translation_button = widgets.Button(
            description='Use Previous Translation',
            disabled=False,
            button_style='success',
            tooltip='Use the translation from the previous version'
        )
        # Event handler
        self.prev_translation_button.on_click(self.use_prev_translation)    
        

        # Initialize other widgets here
        # ...
            

    def on_prev_col_clicked(self, b):
        # Check if we are not in the first column
        if self.current_column_index > 0:
            # Save the current annotation before moving
            self.df.at[self.current_index, self.columns_to_annotate[self.current_column_index]] = self.annotation_input.value

            # Move to the previous column
            self.current_column_index -= 1
            self.display_row(self.current_index, self.current_column_index)
        else:
            # Optionally handle the case when it's already the first column
            print("Already at the first column.")

            
    def get_session_start_index(self):
        try:
            with open(self.history_file_path, 'r') as annotation_file:
                lines = annotation_file.readlines()
                lines_with_index = [line for line in lines if line.startswith("Current Row Index: ")]
                if lines_with_index:
                    last_line = lines_with_index[-1]
                    return int(last_line.split()[-1])
                else:
                    return 0  # Default to 0 if no index is found
        except FileNotFoundError:
            print(f"File not found: {self.history_file_path}")
            return 0  # Default to 0 if file is not found
        pass

    # Display Function: Handles the display of the annotation area and of buttons

    def display_row(self, row_index, col_index):
        self.current_index, self.current_column_index = row_index, col_index
        matching_index = None  # Initialize matching_index
        
        # Get the value from the DataFrame, convert NaN or non-string values to string
        cell_value = self.df.at[row_index, self.columns_to_annotate[col_index]]
        if pd.isna(cell_value):
            cell_value = ''  # Convert NaN to an empty string
        else:
            cell_value = str(cell_value)  # Convert non-string values to string

        clear_output(wait=True)

        # Set the value of the annotation input and display it along with other elements
        self.annotation_input.value = cell_value
        display(self.df.iloc[row_index:row_index+1][self.columns_for_info])
        display(self.df.iloc[row_index:row_index+1][self.columns_line2])
        display(self.df.iloc[row_index:row_index+1][self.columns_line3])
        display(widgets.HTML(value=f"<b>Annotate '{self.columns_to_annotate[col_index]}':</b>"))
        
        # Add the annotation buttons for specific columns
        # Ensure you have methods or logic to handle the display of these buttons
        # Example for 'translation':
        if self.columns_to_annotate[col_index] == 'translation':
            matching_index = self.find_matching_row(self.current_index)
        
        if matching_index is not None:
            display(self.prev_translation_button)
        
        if self.columns_to_annotate[col_index] == "dir_he":
            display(widgets.HBox(self.dir_he_buttons))
        
        if self.columns_to_annotate[col_index] == "cmpl_constr":
            display(widgets.HBox(self.cmpl_constr_buttons))

        if self.columns_to_annotate[col_index] == "cmpl_nt":
            display(widgets.HBox(self.cmpl_nt_buttons))

        if self.columns_to_annotate[col_index] == "cmpl_anim":
            display(widgets.HBox(self.cmpl_anim_buttons))

        if self.columns_to_annotate[col_index] == "cmpl_det":
            display(widgets.HBox(self.cmpl_det_buttons))

        if self.columns_to_annotate[col_index] == "cmpl_indiv":
            display(widgets.HBox(self.cmpl_indiv_buttons))

        if self.columns_to_annotate[col_index] == "cmpl_complex":
            display(widgets.HBox(self.cmpl_complex_buttons))  

        if self.columns_to_annotate[col_index] == 'motion_type':
            display(widgets.HBox(self.motion_type_buttons))

        # Displaying the annotation input and navigation buttons
        display(self.annotation_input)
        display(widgets.HBox([self.prev_col_button, self.next_col_button]))
        display(widgets.HBox([self.prev_row_button, self.next_row_button]))

    # ... [rest of the class] ...

            
    def navigate_row(self, offset):
        new_index = self.current_index + offset
        if 0 <= new_index < len(self.df):
            self.display_row(new_index, 0)

    def find_matching_row(self, df, current_index):
        # Place the code for finding a matching row here
        pass

    def on_prev_col_clicked(self, b):
        # Check if we are not in the first column
        if self.current_column_index > 0:
            # Save the current annotation before moving
            self.df.at[self.current_index, self.columns_to_annotate[self.current_column_index]] = self.annotation_input.value

            # Move to the previous column
            self.current_column_index -= 1
            self.display_row(self.current_index, self.current_column_index)
        else:
            # Optionally handle the case when it's already the first column
            print("Already at the first column.")


    def on_next_col_clicked(self, b):
        # Save the current annotation
        self.df.at[self.current_index, self.columns_to_annotate[self.current_column_index]] = self.annotation_input.value

        # Check if the current column is the last one in the row
        if self.current_column_index + 1 < len(self.columns_to_annotate):
            # Not the last column, move to the next column
            self.current_column_index += 1
            self.display_row(self.current_index, self.current_column_index)
        else:
            # Last column, all annotations for this row are complete
            print("All annotations for this row are complete.")

            
    def on_prev_row_clicked(self, b):
        self.navigate_row(-1)

    def on_next_row_clicked(self, b):
        self.navigate_row(1)
            

    def set_value(self, button):
        # Set the annotation value
        self.annotation_input.value = button.description
        # Save the current annotation
        self.df.at[self.current_index, self.columns_to_annotate[self.current_column_index]] = self.annotation_input.value

        # Move to the next column
        if self.current_column_index + 1 < len(self.columns_to_annotate):
            self.current_column_index += 1
            self.display_row(self.current_index, self.current_column_index)
        else:
            print("All annotations for this row are complete.")
            # Optional: Reset column index or navigate to the next row 
   
   
    def use_prev_translation(self, b):
        matching_index = self.find_matching_row(self.df, self.current_index)
        if matching_index is not None:
            prev_translation = self.df.at[matching_index, 'translation']
            self.annotation_input.value = prev_translation
            self.df.at[self.current_index, 'translation'] = prev_translation
            # Optionally, automatically move to the next column or row here


    def save_annotation_details(self):
        current_datetime = datetime.datetime.now().strftime("%Y-%m-%d %H:%M")
        current_row_index = self.df.index[self.current_index]

        # Calculate the number of rows annotated in this session
        rows_annotated_this_session = self.current_index - self.get_session_start_index() + 1

        details = f"Date and Time: {current_datetime}\nDataFrame: isaiah_dataset\nCurrent Row Index: {current_row_index}\nRows Annotated This Session: {rows_annotated_this_session}\n\n"

        with open(self.history_file_path, 'a') as file:
            file.write(details)

    def save_dataframe(self):
        current_date = datetime.datetime.now().strftime("%Y-%m-%d_%H_%M")

        # Construct the filename
        filename = f"data/annotation_df_history/isaiah_dataset_{current_date}.csv"
        current_file_name = f"data/annotation_df_history/isaiah_dataset.csv"

        # Save the DataFrame to a CSV file
        self.df.to_csv(filename, index=False)
        self.df.to_csv(current_file_name, index=False)

        print(f"DataFrame saved as {filename} and {current_file_name}.")


    def display(self):
        # Initial display of the tool
        self.display_row(self.current_index, self.current_column_index)