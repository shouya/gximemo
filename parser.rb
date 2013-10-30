
require 'strscan'

module GxiMemo
  class Parser
    def parse(text)
      @ss = StringScanner.new(text)
      do_parse
    end

    def match_regexp(regexp)
      return @ss.match(regexp) if @ss.match?(regexp)
      return nil
    end

    def eat_match(match)
      @ss.scan(match)
      return match
    end

    def report_error(exp = '')
      warn 'syntax error'
      abort
    end

    private
    def do_parse
      # space*
      while mtch = space_match; eat(mtch); end
      # "module"
      if mtch = match_text('module')
        eat(mtch)
        (mtch = match_class_name?) ? eat_class_name(mtch) : report_error
      end


    end

    def parse_main

    end

    def space_match
      return match_regexp(/\s\n/)
    end

  end
end


=begin
main
[\x00-\xFF]     : char
"\\n" | "\\t"   : escaped_char
[a-zA-Z_]       : word_beginning
[a-zA-Z0-9_]    : word_middle
'"' (escaped_char | char)* ':'   : literal string
"'" ("\\'" | char)* "'"   : literal string
" " | "\\n" | "\\t" : space_char
word_beginning word_middle* : token
string | token              : element
(element space_char+)* element  : elements
element "{" ruby_code "}"   : element_with_action
char* | "{" ruby_code "}"   : ruby_code

token space_char* ":" space_char* elements space_char* : rule


=end
