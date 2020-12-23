package com.andreystar.grammar;

import java.util.List;

public record ParserRule(String name, List<TypedName> params,
                         List<TypedName> returns,
                         List<RuleRhs> rhs) { }
