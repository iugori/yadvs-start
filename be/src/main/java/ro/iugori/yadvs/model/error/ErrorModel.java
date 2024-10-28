package ro.iugori.yadvs.model.error;

import jakarta.validation.ConstraintViolation;
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
    private String target;
    private TargetType targetType;

    public void setTarget(TargetType type, String name) {
        this.targetType = type;
        this.target = name;
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

    public static ErrorModel of(Throwable t, TargetType targetType, String targetName) {
        var error = new ErrorModel();
        error.setCode(ErrorCode.of(t));
        error.setMessage(t.getMessage());
        error.setTarget(targetType, targetName);
        return error;
    }

    public static <T> ErrorModel of(ConstraintViolation<T> constraint) {
        var error = new ErrorModel();
        error.setCode(ErrorCode.of(constraint.getConstraintDescriptor()));
        error.setMessage(constraint.getMessage());
        error.setTarget(TargetType.FIELD, String.valueOf(constraint.getPropertyPath()));
        return error;
    }

}
