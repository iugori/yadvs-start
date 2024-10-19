package ro.iugori.yadvs.model.error;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.hateoas.RepresentationModel;
import org.springframework.lang.NonNull;

import java.io.Serializable;

@NoArgsConstructor
@Getter
public class ErrorModel extends RepresentationModel<ErrorModel> implements Serializable {

    private String code;
    @Setter
    private String message;
    private ErrorTarget target;

    public void setTarget(TargetType type, String name) {
        this.target = new ErrorTarget(type, name);
    }

    public void setCode(ErrorCode errCode) {
        code = String.format("%d: %s", errCode.code, errCode.name().toLowerCase());
    }

    public int codeAsInt() {
        return Integer.parseInt(getCode().split(":")[0]);
    }

    @Override
    @NonNull
    public String toString() {
        return String.format("%6d: %s", codeAsInt(), message);
    }

}
