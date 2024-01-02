def clean(g_cons):
    """Use to harmonise the DSS content (as strings) with the BHSA content."""
    return g_cons.replace("_", " ").replace("׳", "").replace("'", "")


def find_verb_ref(verb):
    """Returns a list with book, chapter, verse number for a given verb (DSS or BHSA)."""
    if verb.source == "BHSA":
        book = verb.book[0]
        chapter = verb.chapter[0]
        verse_num = verb.verse[0]
    else:
        book = verb.book[0]    
        chapter = verb.chapter[0]
        verse_num = verb.verse[0]
    return [book, chapter, verse_num]


def is_lex_identical(verb_dss): # TODO: handle the "" inside the verse, not only at the end
    """
    Checks if the verses (i.e. BHSA versus DSS) are identical on the lexeme level.
    Remove the empty strings from the DSS verses, if present.
    """
    ref_verb = find_verb_ref(verb_dss)
    
    scroll = verb_dss.to_scrolls.scroll[0]
    
    dss_lex = TFOb.section(ref_verb, DSS, scroll=scroll).to_words.lex
    bhsa_lex = TFOb.section(ref_verb, BHSA).to_words.lex
    
    if dss_lex[-1] == "":
        dss_lex.pop()
    
    return [clean(lex) for lex in bhsa_lex] == [clean(lex) for lex in dss_lex]


def find_bhsa_verb(verb_dss):
    """
    Checks if a verb occurring in DSS also occurs in BHSA (same book, chapter, verse, lexeme).
    Else, returns None.
    """
    
    # Get book chapter verse info from a DSS verb
    ref_dss = find_verb_ref(verb_dss)
    
    if not ref_dss[1].isnumeric():
        # Handles the cases when the chapter in DSS is not a simple number (ex: f14)
        # print("Ref DSS not numeric", ref_dss)
        return 

    # Get the corresponding BHSA verse
    verse_bhsa = TFOb.section(ref_dss, BHSA).to_words
    verb_bhsa = verse_bhsa.filter(lex=verb_dss.lex[0])
    
    # If repetition of verb in same verse: TODO
    if len(verb_bhsa) > 1:
        return # TODO
        scroll = verb_dss.to_scrolls.scroll[0]
        verse_dss = TFOb.section(ref_dss, DSS, scroll)
        print("Verse BHSA:", verse_bhsa)
        print("Verse DSS:", verse_dss)
        
    if verb_bhsa:
        return verb_bhsa
    

def find_clause(verb):
    """Find the complement of a verb. If no match, returns None"""
    if verb.source.name == "BHSA":
        clause = verb.to_clauses.to_clauses
        return clause
    
    # if the verb is not BHSA, it's DSS
    verb_bhsa = find_bhsa_verb(verb)
    scroll = verb.to_scrolls.scroll[0]

    # Check if verses are identical  
    if verb_bhsa and is_lex_identical(verb):
        verse_dss = TFOb.section(find_verb_ref(verb), DSS, scroll=scroll).to_words
        clause_bhsa = find_clause(verb_bhsa)
        
    
        first_word_id = clause_bhsa.to_words.ids[0]
        last_word_id = clause_bhsa.to_words.ids[-1]

        verse_ids = clause_bhsa.to_verses.to_words.ids
        
        try: #TODO TODO TODO
            first_word_index = verse_ids.index(first_word_id)
        except:
            print("Case when clause has no verse (to_verses bugs)", verb_bhsa.ids[0])
            return ""

        #first_word_index = verse_ids.index(first_word_id)
        last_word_index = verse_ids.index(last_word_id)
        
        return verse_dss[first_word_index:last_word_index + 1]

    
def find_complements(verb):
    """Find the complement of a verb. If no match, returns None"""
    if verb.source.name == "BHSA":
        complements = verb.to_clauses.to_phrases.filter(function="Cmpl")
        return complements
    
    # if the verb is not BHSA, it's DSS
    verb_bhsa = find_bhsa_verb(verb)
    scroll = verb.to_scrolls.scroll[0]

    # Check if verses are identical  
    if verb_bhsa and is_lex_identical(verb): # TODO
        verse_dss = TFOb.section(find_verb_ref(verb), DSS, scroll=scroll).to_words
        complements_bhsa = find_complements(verb_bhsa)
        
        complements_dss = []
    
        for complement_bhsa in complements_bhsa:
            first_word_id = complement_bhsa.to_words.ids[0]
            last_word_id = complement_bhsa.to_words.ids[-1]
            
            verse_ids = complement_bhsa.to_verses.to_words.ids
            
            try: #TODO TODO TODO
                first_word_index = verse_ids.index(first_word_id)
            except:
                print("Case when phrase has no verse (to_verses bugs)", verb_bhsa.ids[0])
                return ""
            
            #first_word_index = verse_ids.index(first_word_id)
            last_word_index = verse_ids.index(last_word_id)
            
            complements_dss.append(verse_dss[first_word_index:last_word_index + 1])
        
        return complements_dss
       
        
def find_subject(verb):
    """Find the subject of a verb. If no match, returns None"""
    if verb.source.name == "BHSA":
        subjects = verb.to_clauses.to_phrases.filter(function="Subj")
        assert len(subjects) <= 1
        return subjects
    
    # if the verb is not BHSA, it's DSS
    verb_bhsa = find_bhsa_verb(verb)
    scroll = verb.to_scrolls.scroll[0]

    # Check if verses are identical  
    if verb_bhsa and is_lex_identical(verb): # TODO
        verse_dss = TFOb.section(find_verb_ref(verb), DSS, scroll=scroll).to_words
        subject_bhsa = find_subject(verb_bhsa)
        
        if not subject_bhsa: 
            return ""

        first_word_id = subject_bhsa.to_words.ids[0]
        last_word_id = subject_bhsa.to_words.ids[-1]

        verse_ids = subject_bhsa.to_verses.to_words.ids

        first_word_index = verse_ids.index(first_word_id)
        last_word_index = verse_ids.index(last_word_id)
            
        return verse_dss[first_word_index:last_word_index + 1] 

    
def find_prepositions(verb):
    """Find the complement of a verb. If no match, returns None"""
    if verb.source.name == "BHSA":
        complements = verb.to_clauses.to_phrases.filter(function="Cmpl")
        prepositions = complements.to_words.filter(sp="prep")
        return prepositions
    
    # if the verb is not BHSA, it's DSS
    verb_bhsa = find_bhsa_verb(verb)
    scroll = verb.to_scrolls.scroll[0]

    # Check if verses are identical  
    if verb_bhsa and is_lex_identical(verb): 
        verse_dss = TFOb.section(find_verb_ref(verb), DSS, scroll=scroll).to_words
        prepositions_bhsa = find_prepositions(verb_bhsa)
        
        prepositions_dss = []
    
        for preposition_bhsa in prepositions_bhsa:
            first_word_id = preposition_bhsa.to_words.ids[0]
            last_word_id = preposition_bhsa.to_words.ids[-1]
            
            verse_ids = preposition_bhsa.to_verses.to_words.ids
            
            try: #TODO TODO TODO
                first_word_index = verse_ids.index(first_word_id)
            except:
                print("Case when phrase has no verse (to_verses bugs)", verb_bhsa.ids[0])
                return ""
            
            #first_word_index = verse_ids.index(first_word_id)
            last_word_index = verse_ids.index(last_word_id)
            
            prepositions_dss.append(verse_dss[first_word_index:last_word_index + 1])
        
        return prepositions_dss
    
        
def find_cmpl_anim(cmpl):
    """Based on the nametype value, returns 'anim' or 'inanim' for one complement of a given verb. 
    If the nametype does not provide a clear distinction, returns 'check'.
    If there is no complement to the verb, returns None."""
    
    anim = ["pers", "gens", "god"]
    inanim = ["mens", "topo"]

    nouns = cmpl.to_words.filter_in(sp=["subs", "nmpr"])
    nouns_animacy = []
    for noun in nouns:
        nt = noun.nametype[0]
        if nt in anim:
            nouns_animacy.append("anim")
        elif nt in inanim:
            nouns_animacy.append("inanim")
    return " ".join(nouns_animacy)


def find_cmpl_nametype(cmpl):
    words = cmpl.to_words
    cmpl_nt = []
    for word in words:
        if word.nametype[0]:
            nt = word.nametype[0]
            cmpl_nt.append(nt)
    return " ".join(cmpl_nt)


def find_cmpl_individuation(cmpl):
    """Returns whether the nouns in a cmpl are substantives or proper nouns."""
    cmpl_indiv = []
    for word in cmpl.to_words.filter_in(sp=["subs","nmpr"]):
        cmpl_indiv.append(word.sp[0])
    return " ".join(cmpl_indiv)


def find_cmpl_construction(cmpl):
    """Returns the construction of the complement: vc (for verbal complement), prep for prepositional complement
    and dir_he for complement with directive-he (or a combination of those)."""
    
    cmpl_construction = []
    
    for word in cmpl.to_words:
        if word.filter(sp="prep"):
            cmpl_construction.append("prep")
        elif "H" in word.uvf:
            cmpl_construction.append("dir_he")
            
    if cmpl_construction == []:
        return "vc"
    else:
        return " + ".join(cmpl_construction)
    