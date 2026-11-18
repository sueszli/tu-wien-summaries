package dslab;

import org.hamcrest.Factory;
import org.hamcrest.Matcher;
import org.hamcrest.core.SubstringMatcher;

/**
 * String matcher that checks for regex matching.
 */
public class StringMatches extends SubstringMatcher {

    public StringMatches(String substring) {
        super(substring);
    }

    @Factory
    public static Matcher<String> matchesPattern(String pattern) {
        return new StringMatches(pattern);
    }

    @Override
    protected boolean evalSubstringOf(String string) {
        return string.matches(substring);
    }

    @Override
    protected String relationship() {
        return "matching";
    }
}
