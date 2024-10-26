package ro.iugori.yadvs.model.rest.shared;

import com.fasterxml.jackson.annotation.JsonInclude;
import jakarta.annotation.Nonnull;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.*;
import ro.iugori.yadvs.model.entity.PollOptionEntity;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class PollOption implements Comparable<PollOption> {

    private Long id;

    private Short position;

    @NotNull
    @NotEmpty
    @Size(max = PollOptionEntity.DESCRIPTION_LENGTH)
    private String description;

    @Override
    public int compareTo(@Nonnull PollOption other) {
        if (this.position == null) {
            if (other.position == null) {
                return 0;
            }
            return 1;
        }
        if (other.position == null) {
            return -1;
        }
        return Short.compare(this.position, other.position);
    }

}
