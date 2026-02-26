package zio.bdd.core;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD, ElementType.METHOD})
public @interface Suite {
    /**
     * One or more feature directories or individual .feature file paths.
     * Replaces the old singular {@code featureDir()} — accepts multiple directories:
     * {@code featureDirs = {"src/test/resources/features/components", "src/test/resources/features/liveDependency"}}
     */
    String[] featureDirs() default {"src/test/resources/features"};

    /** @deprecated Use {@link #featureDirs()} instead. Kept for backward compatibility; takes effect only when featureDirs is not set. */
    @Deprecated
    String featureDir() default "";

    String[] reporters() default {"pretty"};
    int parallelism() default 1;
    /**
     * Max concurrent scenarios per feature. 0 = auto (use available processors).
     * Defaults to 0 so parallel scenario execution is on by default.
     */
    int scenarioParallelism() default 0;
    String[] includeTags() default {};
    String[] excludeTags() default {};
    String logLevel() default "info";

    /** Step execution timeout in seconds. 0 means no timeout (default). */
    int stepTimeout() default 0;
}
