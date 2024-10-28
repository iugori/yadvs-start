package ro.iugori.yadvs.util.mapping;

import ro.iugori.yadvs.model.entity.PollOptionEntity;
import ro.iugori.yadvs.model.rest.shared.PollOption;

public class PollOptionMapper {

    public static void putDto2EntityUserInput(PollOption dto, PollOptionEntity entity) {
        entity.setDescription(dto.getDescription());
    }

    public static PollOption dtoFrom(PollOptionEntity entity) {
        var dto = new PollOption();
        dto.setId(entity.getId());
        dto.setPosition(entity.getPosition());
        dto.setDescription(entity.getDescription());
        return dto;
    }

}
