import ipywidgets as widgets
import pandas as pd
import datetime
from IPython.display import display, clear_output
from ipywidgets import VBox, HBox, Button, Text, Label, HTML, IntText, Textarea

class AnnotationTool:
    def __init__(self, csv_file_path, history_file_path):
        # Load and configure the DataFrame
        self.df = pd.read_csv(csv_file_path).fillna("")
        self.history_file_path = history_file_path
        self.current_index = self.get_session_start_index()
        self.current_column_index = 0
        self.configure_dataframe()

        # Define column groups
        self.columns_for_info = [
            'verb_id', 'lex', 'scroll', 'book', 'chapter', 'verse_num',
            'gcons_verb', 'gcons_verse', 'sign_info', 'stem', 'tense'
        ]
        self.columns_line2 = [
            'gcons_clause', 'subject', 'complement', 'cmpl_lex', 'cmpl_translation',
            'dir_he', 'cmpl_constr', 'cmpl_nt', 'cmpl_anim', 'cmpl_det',
            'cmpl_indiv', 'cmpl_complex',
        ]
        self.columns_line3 = [
            'motion_type', 'spatial_arg_type', 'preposition_1', 'preposition_2',
            'preposition_3', 'preposition_4', 'comments', 'reconstructed_verse',
        ]
        self.columns_to_annotate = self.columns_line2 + self.columns_line3

        # Initialize buttons and widgets
        self.column_buttons = {}
        self.annotation_buttons = {}
        self.initialize_widgets()

        # Initialize annotation input area
        self.annotation_input = self.create_textarea(
            placeholder='Type your annotation here',
            description='',
            layout=widgets.Layout(width='100%', height='100px')
        )

    def configure_dataframe(self):
        pd.set_option('display.max_colwidth', None)
        pd.set_option('display.max_columns', None)
        pd.set_option('display.max_rows', None)

        # Ensure 'dir_he' is properly formatted
        self.df["dir_he"] = self.df["dir_he"].astype(str).replace({"1.0": "1", "0.0": "0"})

        # Reorder DataFrame columns
        ordered_columns = [
            'verb_id', 'lex', 'scroll', 'book', 'chapter', 'verse_num',
            'gcons_verb', 'gcons_verse', 'sign_info', 'stem', 'tense',
            'gcons_clause', 'subject', 'complement', 'cmpl_lex', "cmpl_translation",
            'dir_he', 'cmpl_constr', 'cmpl_nt', 'cmpl_anim', 'cmpl_det',
            'cmpl_indiv', 'cmpl_complex', 'motion_type', 'spatial_arg_type',
            'preposition_1', 'preposition_2', 'preposition_3', 'preposition_4',
            'comments', 'reconstructed_verse',
        ]
        self.df = self.df[ordered_columns]

    def create_textarea(self, **kwargs):
        return Textarea(**kwargs)

    def create_text_input(self, description):
        return Text(description=description)

    def create_button(self, description, button_style='', tooltip='', icon='', on_click=None):
        button = Button(
            description=description,
            button_style=button_style,
            tooltip=tooltip,
            icon=icon if icon is not None else ''  # Ensure icon is a string; default to ''
        )
        if on_click:
            button.on_click(on_click)
        return button

    def create_button_group(self, descriptions, button_style='', tooltip_prefix='', on_click=None):
        buttons = []
        for desc in descriptions:
            tooltip = f"{tooltip_prefix}: {desc}" if tooltip_prefix else ''
            button = self.create_button(
                description=desc,
                button_style=button_style,
                tooltip=tooltip,
                on_click=on_click
            )
            buttons.append(button)
        return buttons

    def initialize_widgets(self):
        # Create user input widgets for adding new rows
        self.user_inputs = VBox([
            self.create_text_input('verb_id:'),
            self.create_text_input('lex:'),
            self.create_text_input('gcons_verb:'),
            self.create_text_input('stem:'),
            self.create_text_input('tense:'),
            self.create_text_input('gcons_clause:'),
            self.create_text_input('subject:'),
            self.create_text_input('complement:'),
            self.create_text_input('cmpl_lex:')
        ])

        # Create action buttons
        self.add_row_button = self.create_button(
            description='Add row',
            button_style='success',
            tooltip='Click to add a new row',
            on_click=self.on_add_row_button_clicked
        )

        self.matching_columns_button = self.create_button(
            description='Fill columns',
            button_style='success',
            tooltip='Click to annotate matching columns based on a previous row',
            on_click=self.annotate_matching_columns
        )

        # Navigation buttons
        self.navigation_buttons = {
            'prev_col': self.create_button(
                description='Previous Column',
                button_style='primary',
                tooltip='Go to the previous column',
                icon='arrow-left',
                on_click=self.on_prev_col_clicked
            ),
            'next_col': self.create_button(
                description='Next Column',
                button_style='info',
                tooltip='Go to the next column',
                icon='arrow-right',
                on_click=self.on_next_col_clicked
            ),
            'prev_row': self.create_button(
                description='Previous Row',
                button_style='warning',
                tooltip='Go to the previous row',
                icon='arrow-left',
                on_click=self.on_prev_row_clicked
            ),
            'next_row': self.create_button(
                description='Next Row',
                button_style='success',
                tooltip='Go to the next row',
                icon='arrow-right',
                on_click=self.on_next_row_clicked
            )
        }

        self.navigation_box = HBox([
            self.navigation_buttons['prev_col'],
            self.navigation_buttons['next_col'],
            self.navigation_buttons['prev_row'],
            self.navigation_buttons['next_row']
        ])

        # Create annotation-specific buttons based on columns
        self.create_annotation_specific_buttons()

        # Create column navigation buttons
        self.create_column_navigation_buttons()

        # Create translation and comment buttons
        self.translation_buttons = {
            'prev_translation': self.create_button(
                description='Use Previous Translation',
                tooltip='Use the translation from the other version',
                on_click=self.use_prev_translation
            ),
            'prev_cmpl_lex': self.create_button(
                description='Use Previous cmpl_lex',
                tooltip='Use the cmpl_lex from the other version',
                on_click=self.use_prev_cmpl_lex
            ),
            'prev_comment': self.create_button(
                description='Use Previous Comment',
                tooltip='Use the comment from the other version',
                on_click=self.use_prev_comment
            )
        }

        # Initialize jump to index widget
        self.display_jump_widget()

    def create_annotation_specific_buttons(self):
        # Define button configurations for each annotatable column
        button_configs = {
            'complement': {
                'descriptions': ['no complement'],
                'button_style': '',
                'tooltip_prefix': '',
                'on_click': self.set_value
            },
            'dir_he': {
                'descriptions': ['0', '1'],
                'button_style': '',
                'tooltip_prefix': 'Set dir_he',
                'on_click': self.set_value
            },
            'cmpl_constr': {
                'descriptions': ['prep', 'dir-he', 'vc', 'prep + dir-he', 'prep + prep'],
                'button_style': '',
                'tooltip_prefix': 'Set cmpl_constr',
                'on_click': self.set_value
            },
            'cmpl_nt': {
                'descriptions': [
                    'abs', 'adv', 'bopa', 'dir', 'gens', 'obj', 'occ', 'other',
                    'pers', 'phen', 'ppde', 'ppin', 'prps', 'topo'
                ],
                'button_style': '',
                'tooltip_prefix': 'Set cmpl_nt',
                'on_click': self.set_value
            },
            'cmpl_anim': {
                'descriptions': ['anim', 'inanim', 'anim inanim'],
                'button_style': '',
                'tooltip_prefix': 'Set cmpl_anim',
                'on_click': self.set_value
            },
            'cmpl_det': {
                'descriptions': ['det', 'und', 'na'],
                'button_style': '',
                'tooltip_prefix': 'Set cmpl_det',
                'on_click': self.set_value
            },
            'cmpl_indiv': {
                'descriptions': ['subs', 'nmpr', 'prsf', 'ppde', 'ppin', 'adj', 'adv'],
                'button_style': '',
                'tooltip_prefix': 'Set cmpl_indiv',
                'on_click': self.set_value
            },
            'cmpl_complex': {
                'descriptions': ['simple', 'complex'],
                'button_style': '',
                'tooltip_prefix': 'Set cmpl_complex',
                'on_click': self.set_value
            },
            'motion_type': {
                'descriptions': ['factive', 'fictive', 'vertical', 'posture/not motion'],
                'button_style': '',
                'tooltip_prefix': 'Set motion_type',
                'on_click': self.set_value
            },
            'spatial_arg_type': {
                'descriptions': [
                    'goal', 'trajectory', 'goal/traj', 'goal/recipient',
                    'location', 'source', 'other'
                ],
                'button_style': '',
                'tooltip_prefix': 'Set spatial_arg_type',
                'on_click': self.set_value
            },
            'comments': {
                'descriptions': ['reconstructed', 'reconstructed?', 'min excluded'],
                'button_style': '',
                'tooltip_prefix': 'Set comments',
                'on_click': self.set_value
            }
        }

        # Create button groups for each column
        for column, config in button_configs.items():
            self.annotation_buttons[column] = self.create_button_group(
                descriptions=config['descriptions'],
                button_style=config['button_style'],
                tooltip_prefix=config['tooltip_prefix'],
                on_click=config['on_click']
            )

    def create_column_navigation_buttons(self):
        # Create buttons to navigate to specific columns
        for column_name in self.columns_to_annotate:
            self.column_buttons[column_name] = self.create_button(
                description=column_name,
                button_style='info',
                tooltip=f'Go to the {column_name} column',
                on_click=self.go_to_on_clicked
            )

    def on_prev_col_clicked(self, b):
        self.navigate_column(-1)

    def on_next_col_clicked(self, b):
        self.navigate_column(1)

    def on_prev_row_clicked(self, b):
        self.navigate_row(-1)

    def on_next_row_clicked(self, b):
        self.navigate_row(1)

    def navigate_column(self, offset):
        new_column_index = self.current_column_index + offset
        if 0 <= new_column_index < len(self.columns_to_annotate):
            self.save_current_annotation()
            self.current_column_index = new_column_index
            self.display_row(self.current_index, self.current_column_index)
        else:
            print("Cannot navigate beyond the available columns.")

    def navigate_row(self, offset):
        new_index = self.current_index + offset
        if 0 <= new_index < len(self.df):
            self.save_current_annotation()
            self.current_index = new_index
            self.current_column_index = 0
            self.display_row(self.current_index, self.current_column_index)
        else:
            print("Cannot navigate beyond the available rows.")

    def save_current_annotation(self):
        current_column = self.columns_to_annotate[self.current_column_index]
        current_value = self.df.at[self.current_index, current_column]
        new_value = self.annotation_input.value

        # Only save if there's a change
        if new_value != str(current_value):
            self.df.at[self.current_index, current_column] = new_value

    def display_row(self, row_index, col_index):
        # Do NOT save the current annotation here
        self.current_index, self.current_column_index = row_index, col_index
        clear_output(wait=True)

        # Display row information
        display(Label('Display row method called'))
        display(self.df.iloc[row_index:row_index+1][self.columns_for_info])
        display(self.df.iloc[row_index:row_index+1][self.columns_line2])
        display(self.df.iloc[row_index:row_index+1][self.columns_line3])

        # Display column navigation buttons
        display(HTML("<b>Go to column:</b>"))
        display(HBox(list(self.column_buttons.values())))

        # Display current annotation details
        current_column = self.columns_to_annotate[col_index]
        display(HTML(f"<b>Annotate '{current_column}':</b>"))

        # Display matching columns button if applicable
        matching_index = self.find_matching_row()
        if matching_index is not None:
            display(self.matching_columns_button)

        # Display specific buttons based on the current column
        self.display_specific_buttons(current_column, matching_index)

        # Set the annotation input value
        cell_value = str(self.df.at[row_index, current_column]) if not pd.isna(self.df.at[row_index, current_column]) else ''
        self.annotation_input.value = cell_value
        display(self.annotation_input)

        # Display navigation buttons
        display(self.navigation_box)

    def display_specific_buttons(self, current_column, matching_index):
        # Mapping of columns to their respective button groups or single buttons
        specific_display = {
            "complement": HBox(self.annotation_buttons.get('complement', [])),
            "dir_he": HBox(self.annotation_buttons.get('dir_he', [])),
            "cmpl_constr": HBox(self.annotation_buttons.get('cmpl_constr', [])),
            "cmpl_nt": HBox(self.annotation_buttons.get('cmpl_nt', [])),
            "cmpl_anim": HBox(self.annotation_buttons.get('cmpl_anim', [])),
            "cmpl_det": HBox(self.annotation_buttons.get('cmpl_det', [])),
            "cmpl_indiv": HBox(self.annotation_buttons.get('cmpl_indiv', [])),
            "cmpl_complex": HBox(self.annotation_buttons.get('cmpl_complex', [])),
            "motion_type": HBox(self.annotation_buttons.get('motion_type', [])),
            "spatial_arg_type": HBox(self.annotation_buttons.get('spatial_arg_type', [])),
            "comments": HBox(self.annotation_buttons.get('comments', [])),
            # Handle columns with previous value buttons
            "cmpl_lex": self.get_previous_value_button('cmpl_lex', matching_index, self.translation_buttons['prev_cmpl_lex']),
            "cmpl_translation": self.get_previous_value_button('cmpl_translation', matching_index, self.translation_buttons['prev_translation']),
            "comments_specific": self.get_previous_value_button('comments', matching_index, self.translation_buttons['prev_comment']),
            # Add 'gcons_clause' if specific buttons are needed (currently none)
            # If no specific buttons, nothing is displayed
        }

        # Determine which specific buttons to display
        button_widget = specific_display.get(current_column)
        if button_widget:
            display(button_widget)

    def get_previous_value_button(self, column, matching_index, button):
        if matching_index is not None:
            prev_value = self.df.at[matching_index, column]
            button.description = f"Previous {column}: {prev_value}"
            button.layout.width = '500px' if column != 'comments' else '800px'
            return button
        return None

    def create_column_navigation_buttons(self):
        # Create buttons to navigate to specific columns
        for column_name in self.columns_to_annotate:
            self.column_buttons[column_name] = self.create_button(
                description=column_name,
                button_style='info',
                tooltip=f'Go to the {column_name} column',
                on_click=self.go_to_on_clicked
            )

    def go_to_on_clicked(self, button):
        column_name = button.description
        if column_name in self.columns_to_annotate:
            self.save_current_annotation()
            self.current_column_index = self.columns_to_annotate.index(column_name)
            self.display_row(self.current_index, self.current_column_index)
        else:
            print(f"Column '{column_name}' not found in the dataframe.")

    def find_matching_row(self):
        current_row = self.df.iloc[self.current_index]
        matching_criteria = ['lex', 'book', 'stem', 'tense']
        current_chapter = str(current_row['chapter'])
        current_verse = str(current_row['verse_num'])

        # Search up to 10 rows back for a matching row
        start_index = max(0, self.current_index - 10)
        for index in range(self.current_index - 1, start_index - 1, -1):
            row = self.df.iloc[index]
            if (
                row['scroll'] != current_row['scroll'] and
                all(str(row[crit]) == str(current_row[crit]) for crit in matching_criteria) and
                str(row['chapter']) == current_chapter and
                str(row['verse_num']) == current_verse
            ):
                return index
        return None

    def annotate_matching_columns(self, b):
        matching_index = self.find_matching_row()
        if matching_index is not None:
            columns_to_copy = [
                'cmpl_lex', 'cmpl_translation', 'cmpl_constr', 'cmpl_nt',
                'cmpl_anim', 'cmpl_det', 'cmpl_indiv', 'cmpl_complex',
                'motion_type', 'spatial_arg_type', 'preposition_1',
                'preposition_2', 'preposition_3', 'preposition_4', 'comments'
            ]
            for col in columns_to_copy:
                self.df.at[self.current_index, col] = self.df.at[matching_index, col]
            self.display_row(self.current_index, self.current_column_index)
        else:
            print("No matching row found to copy annotations from.")

    def on_add_row_button_clicked(self, b):
        new_row_data = {
            'verb_id': self.user_inputs.children[0].value,
            'lex': self.user_inputs.children[1].value,
            'gcons_verb': self.user_inputs.children[2].value,
            'stem': self.user_inputs.children[3].value,
            'tense': self.user_inputs.children[4].value,
            'gcons_clause': self.user_inputs.children[5].value,
            'subject': self.user_inputs.children[6].value,
            'complement': self.user_inputs.children[7].value,
            'cmpl_lex': self.user_inputs.children[8].value,
        }

        self.add_new_row(new_row_data)
        print(f'A new row has been added at index {self.current_index + 1} with the following information:\n')
        print(f'''
              verb_id: {new_row_data['verb_id']}, 
              lex: {new_row_data['lex']}, 
              gcons_verb: {new_row_data['gcons_verb']}, 
              stem: {new_row_data['stem']}, 
              tense: {new_row_data['tense']}
              ''')

    def add_new_row(self, new_row_data):
        columns_to_copy = ['scroll', 'book', 'chapter', 'verse_num', 'gcons_verse', "sign_info"]
        new_row = {col: self.df.at[self.current_index, col] for col in columns_to_copy}

        # Update with new row data
        new_row.update(new_row_data)

        # Fill missing columns with empty strings
        for col in self.df.columns:
            new_row.setdefault(col, '')

        # Insert the new row
        new_df = pd.DataFrame([new_row])
        self.df = pd.concat(
            [self.df.iloc[:self.current_index + 1], new_df, self.df.iloc[self.current_index + 1:]]
        ).reset_index(drop=True)

    def delete_row(self, index):
        if index in self.df.index:
            self.df = self.df.drop(index).reset_index(drop=True)
            print(f"Row at index {index} has been deleted.")
        else:
            print("Invalid index. No row deleted.")

    def get_session_start_index(self):
        try:
            with open(self.history_file_path, 'r') as annotation_file:
                lines = annotation_file.readlines()
                lines_with_index = [line for line in lines if line.startswith("Current Row Index: ")]
                if lines_with_index:
                    last_line = lines_with_index[-1]
                    return int(last_line.split()[-1])
        except FileNotFoundError:
            print(f"File not found: {self.history_file_path}")
        return 0

    def display_jump_widget(self):
        if not hasattr(self, 'index_input') or not hasattr(self, 'jump_button'):
            self.index_input = IntText(
                value=self.current_index,
                description='Index:',
                layout=widgets.Layout(width='200px'),
            )
            self.jump_button = Button(description="Jump to Index")
            self.jump_button.on_click(self.on_jump_button_clicked)
        display(HBox([self.index_input, self.jump_button]))

    def on_jump_button_clicked(self, button):
        desired_index = self.index_input.value
        if 0 <= desired_index < len(self.df):
            self.save_current_annotation()
            self.current_index = desired_index
            self.current_column_index = 0
            self.display_row(self.current_index, self.current_column_index)
            print(f"Jumped to row {self.current_index}.")
        else:
            print("Index out of range. Please enter a valid index.")

    def set_value(self, button):
        new_value = button.description
        current_column = self.columns_to_annotate[self.current_column_index]
        self.annotation_input.value = new_value
        self.df.at[self.current_index, current_column] = new_value
        self.navigate_column(1)

    def use_prev_translation(self, b):
        self.use_previous_value('cmpl_translation')

    def use_prev_cmpl_lex(self, b):
        self.use_previous_value('cmpl_lex')

    def use_prev_comment(self, b):
        self.use_previous_value('comments')

    def use_previous_value(self, column):
        matching_index = self.find_matching_row()
        if matching_index is not None:
            prev_value = self.df.at[matching_index, column]
            self.annotation_input.value = prev_value
            self.df.at[self.current_index, column] = prev_value
            self.current_column_index += 1
            self.display_row(self.current_index, self.current_column_index)
        else:
            print(f"No previous value found for {column}.")

    def save_annotation_details(self):
        current_datetime = datetime.datetime.now().strftime("%Y-%m-%d %H:%M")
        current_row_index = self.df.index[self.current_index]
        rows_annotated_this_session = self.current_index - self.get_session_start_index() + 1

        details = (
            f"Date and Time: {current_datetime}\n"
            f"DataFrame: Exodus_dataset\n"
            f"Current Row Index: {current_row_index}\n"
            f"Rows Annotated This Session: {rows_annotated_this_session}\n\n"
        )

        with open(self.history_file_path, 'a') as file:
            file.write(details)

    def save_dataframe(self, historical_filename, current_filename):
        timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
        historical_file = f"{historical_filename}_{timestamp}.csv"
        self.df.to_csv(historical_file, index=False)
        self.df.to_csv(current_filename, index=False)
        print(f"DataFrame saved as {historical_file} and {current_filename}.")

    def display(self):
        self.display_row(self.current_index, self.current_column_index)
