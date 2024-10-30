package ro.iugori.yadvs.model.business;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.*;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.hateoas.RepresentationModel;
import ro.iugori.yadvs.model.entity.PollEntity;
import ro.iugori.yadvs.delegate.validation.XFieldComparison;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Builder
@XFieldComparison(field1 = "start", rel = "lt", field2 = "end")
@JsonInclude(JsonInclude.Include.NON_NULL)
public class Poll extends RepresentationModel<Poll> {

    public record OptionsWrapper(List<PollOption> optionList) {
    }

    @Getter
    @Setter
    private Long id;

    @NotNull
    @NotEmpty
    @Size(max = PollEntity.NAME_LENGTH)
    @Getter
    @Setter
    private String name;

    @NotNull
    @NotEmpty
    @Size(max = PollEntity.DESCRIPTION_LENGTH)
    @Getter
    @Setter
    private String description;

    @Getter
    @Setter
    private PollStatus status;

    @Getter
    @Setter
    private Boolean multiOption;

    @Getter
    @Setter
    private LocalDateTime start;

    @Getter
    @Setter
    private LocalDateTime end;

    private List<PollOption> options;

    public void fillOptions(Collection<PollOption> options) {
        if (this.options == null) {
            this.options = new ArrayList<>();
        }
        this.options.addAll(options);
    }

    @JsonProperty("_embedded")
    public OptionsWrapper getOptions() {
        if (CollectionUtils.isEmpty(options)) {
            return null;
        }
        return new OptionsWrapper(options);
    }

}
