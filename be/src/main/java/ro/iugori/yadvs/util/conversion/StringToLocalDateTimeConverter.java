package ro.iugori.yadvs.util.conversion;

import jakarta.annotation.Nonnull;
import org.springframework.core.convert.converter.Converter;
import org.springframework.util.StringUtils;

import java.time.LocalDateTime;

public class StringToLocalDateTimeConverter implements Converter<String, LocalDateTime> {

    @Override
    public LocalDateTime convert(@Nonnull String source) {
        if (StringUtils.hasText(source)) {
            source = source.trim();
        }
        return LocalDateTime.parse(source);
    }

}
