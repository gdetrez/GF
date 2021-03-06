#ifndef PGF_LEXER_H_
#define PGF_LEXER_H_

#include <gu/read.h>

/// A single lexical token			      
typedef GuString PgfToken;

typedef struct PgfLexer PgfLexer;

PgfLexer*
pgf_new_lexer(GuReader *rdr, GuPool *pool);

PgfToken
pgf_lexer_read_token(PgfLexer *lexer, GuExn* err);

PgfToken
pgf_lexer_current_token(PgfLexer *lexer);

#endif // PGF_LEXER_H_
