package zio.bdd.core;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD, ElementType.METHOD})
public @interface Suite {
    String featureDir() default "src/test/resources/features";
    String[] reporters() default {"console"};
    int parallelism() default 1;
    String[] includeTags() default {};
    String[] excludeTags() default {};
    String logLevel() default "info";
}